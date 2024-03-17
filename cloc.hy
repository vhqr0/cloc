#!/usr/bin/env hy

(require
  dash *)

(import
  dash *
  dash.strtools :as s
  dash.operator :as o
  collections [Counter]
  pathlib [Path]
  chardet [detect :as char-detect])

(defn #^ (py "list[str]") decode-code [#^ bytes code]
  "Decode code from bytes to list of lines with auto encoding and newline detection."
  (let [detect (char-detect code)
        encoding (get detect "encoding")]
    (->> (s.decode code (if (none? encoding) "utf-8" encoding))
         (s.split-lines)
         (--map (+ it "\n"))
         list)))

(defn parse-code [parser #^ (py "list[str]") code]
  "Parse code (list of lines) to AST Tree."
  (let [readfn (fn [_ point]
                 (let [#(row col) point]
                   (when (< row (len code))
                     (let [line (get code row)]
                       (when (< col (len line))
                         (s.encode (cut line col None)))))))]
    (.parse parser readfn)))

(defn node-iter [node]
  "Iter children of AST Node."
  (yield node)
  (yield-from (-concat-in (-map node-iter node.children))))

(defn tree-iter [tree]
  "Iter children of AST Tree."
  (node-iter tree.root-node))

(defn region-iter [tree types]
  "Iter regions from AST Tree by types."
  (->> (tree-iter tree)
       (--filter (in it.type types))
       (--map #(it.start-point it.end-point))))

(defn flatten-line-regions [code regions]
  "Flatten cross lines regions to inline regions. eg.
(flatten-line-regions [\"...\" \"...\"] [#(#(0 0) #(1 2)) #(#(1 10) #(1 20))])
;; => [[#(0 ...)] [#(0 2) #(10 20)]]
"
  (let [line-regions (list (-repeatedly-n (len code) list))]
    (--each regions
            (let [#(#(start-row start-col) #(end-row end-col)) it]
              (while (< start-row end-row)
                (let [end-col (len (get code start-row))]
                  (when (< start-col end-col)
                    (-update! line-regions start-row -conj! #(start-col end-col))))
                (+= start-row 1)
                (setv start-col 0))
              (when (< start-col end-col)
                (-update! line-regions start-row -conj! #(start-col end-col)))))
    line-regions))

(defn select-region [line region]
  (->> region
       (--map
         (let [#(start end) it]
           (cut line start end)))
       (s.concats-in)))

(defn delete-region [line region]
  (->> (enumerate line)
       (--remove
         (let [#(index char) it]
           (--any? (let [#(start end) it]
                     (chainc start <= index < end))
                   region)))
       (--map
         (let [#(index char) it]
           char))
       (s.concats-in)))

(defn dir-path-iter [path]
  "Iter files from dir."
  (--mapcat
    (let [#(base dirs files) it]
      (--map (/ base it) files))
    (.walk path)))

(defn path-iter [path]
  "Iter files from path."
  (when (str? path)
    (setv path (Path path)))
  (cond (.is-dir path) (yield-from (dir-path-iter path))
        (.is-file path) (yield path)))

(defn src-iter [paths extensions]
  "Iter source files from paths by extensions."
  (->> (-map path-iter paths)
       -concat-in
       (--filter (in it.suffix extensions))))



(defclass SrcCounter []
  (setv src-extensions None
        ts-language None
        ts-comment-types #("comment"))

  (setv override-extensions None
        override-languages None)

  (setv extension-dict (dict)
        language-dict (dict))

  (defn __init-subclass__ [cls #* args #** kwargs]
    "Register extensions/languages when create subclass. Respect override-extensions/languages."
    (.__init-subclass__ (super) #* args #** kwargs)
    (when-let [extensions (if (none? cls.override-extensions) cls.src-extensions cls.override-extensions)]
      (--each extensions (-assoc! cls.extension-dict it cls)))
    (when-let [languages (if (none? cls.override-languages) [cls.ts-language] cls.override-languages)]
      (--each languages (-assoc! cls.language-dict it cls))))

  (defn [classmethod] from-extension [cls extension #* args #** kwargs]
    ((get cls.extension-dict extension) #* args #** kwargs))

  (defn [classmethod] from-language [cls language #* args #** kwargs]
    ((get cls.language-dict language) #* args #** kwargs))

  (defn __init__ [self [parser None] [buffer-size (do-mac (* 1024 1024 1024))]]
    (setv self.buffer-size buffer-size))

  (defn [property] ts-parser [self]
    "Get Tree Sitter parser by self.ts-language."
    (import tree-sitter-languages [get-parser])
    (get-parser self.ts-language))

  (defn count-code [self #^ bytes code]
    "Count lines of bytes: decode bytes, parse AST Tree, check parse error, then count."
    (let [code (decode-code code)
          tree (parse-code self.ts-parser code)]
      ;; check cross lines parse error
      (when-let [region (->> (region-iter tree #("ERROR"))
                             (--filter
                               (let [#(#(start-row start-col) #(end-row end-col)) it]
                                 (!= start-row end-row)))
                             first)]
        (raise (RuntimeError (s.format "parse error {}" region))))
      (let [comment-regions (->> (region-iter tree self.ts-comment-types)
                                 (flatten-line-regions code))]
        (Counter (--map
                   (let [#(line comment-region) it]
                     (cond (s.blank? (s.strip line)) "blank"
                           (and comment-region (s.blank? (s.strip (delete-region line comment-region)))) "comment"
                           True "code"))
                   (-zip code comment-regions))))))

  (defn count-file [self path]
    "Count lines of file: check file size first."
    (if (> (. (.stat path) st-size) self.buffer-size)
        (raise (RuntimeError "file too large"))
        (let [code (.read (.open path "rb"))]
          (.count-code self code))))

  (defn count-files-from-iter [self paths [logger None]]
    (->> paths
         (--keep
           (try
             (.count-file self it)
             (except [e Exception]
               (when logger
                 (.info logger "except while counting %s %s" it e))
               None)))
         (--reduce-from
           (-merge-with o.add acc it {"file" 1})
           (dict))))

  (defn count-src [self paths [logger None]]
    (.count-files-from-iter self (src-iter paths self.src-extensions) :logger logger))

  (defn multi-count-src [self paths n [logger None]]
    (import
      ;; queue [Queue]
      ;; threading [Thread]
      multiprocessing [Queue Process :as Thread])

    (setv tasks (Queue (* 2 n))
          results (Queue))

    (defn counter []
      (let [it (-take-while (-notfn none?) (-repeatedly tasks.get))]
        (-> (.count-files-from-iter self it :logger logger)
            (results.put))))

    (let [threads (list (--repeatedly-n n (Thread :target counter)))]
      (--each threads (.start it))
      (-each
        (-concat (src-iter paths self.src-extensions) (-repeat-n n None))
        tasks.put)
      (--each threads (.join it)))

    (-merge-with o.add #* (-repeatedly-n n results.get-nowait))))



(defclass PythonCounter [SrcCounter]
  (setv src-extensions #(".py")
        ts-language "python"
        override-languages #("python" "py")))

(defclass CppCounter [SrcCounter]
  (setv src-extensions #(".c" ".cc" ".cpp")
        ts-language "cpp"
        override-languages #("c" "cc" "cpp")))

(defclass HppCounter [SrcCounter]
  (setv src-extensions #(".h" ".hh" ".hpp")
        ts-language "cpp"
        override-languages #("h" "hh" "hpp")))

(defclass CppHppCounter [SrcCounter]
  (setv src-extensions #(".c" ".h" ".cc" ".hh" ".cpp" ".hpp")
        ts-language "cpp"
        override-extensions #()
        override-languages #("c+h")))

(defclass JavascriptCounter [SrcCounter]
  (setv src-extensions #(".js")
        ts-language "javascript"
        override-languages #("javascript" "js")))

(defclass TypescriptCounter [SrcCounter]
  (setv src-extensions #(".ts" ".js")
        ts-language "typescript"
        override-extensions #(".ts")
        override-languages #("typescript" "ts")))

(defclass TSXCounter [SrcCounter]
  (setv src-extensions #(".ts" ".js" ".tsx" ".jsx")
        ts-language "tsx"
        override-extensions #(".tsx" ".jsx")
        override-languages #("tsx" "jsx")))

(defclass GoCounter [SrcCounter]
  (setv src-extensions #(".go")
        ts-language "go"
        override-languages #("go" "golang")))

(defclass RustCounter [SrcCounter]
  (setv src-extensions #(".rs")
        ts-language "rust"
        override-languages #("rust" "rs")))

(defclass JavaCounter [SrcCounter]
  (setv src-extensions #(".java")
        ts-language "java"))

(defclass CSharpCounter [SrcCounter]
  (setv src-extensions #(".cs")
        ts-language "c_sharp"
        override-languages #("c_sharp" "csharp" "cs")))

(defclass RubyCounter [SrcCounter]
  (setv src-extensions #(".rb")
        ts-language "ruby"
        override-languages #("rb" "ruby")))

(defclass PerlCounter [SrcCounter]
  (setv src-extensions #(".pl")
        ts-language "perl"
        override-languages #("perl" "pl")))

(defclass LuaCounter [SrcCounter]
  (setv src-extensions #(".lua")
        ts-language "lua"))

(defclass HaskellCounter [SrcCounter]
  (setv src-extensions #(".hs")
        ts-language "haskell"
        override-languages #("haskell" "hs")))

(defclass ElispCounter [SrcCounter]
  (setv src-extensions #(".el")
        ts-language "elisp"
        override-languages #("elisp" "el")))



(defn cloc-get-logger []
  (import logging)
  (logging.basicConfig :level "INFO")
  (logging.getLogger "cloc"))

(setv cloc-template "\
-----------------------------------------------------------
 Files        Lines         Code     Comments       Blanks
-----------------------------------------------------------
{:6d}     {:8d}     {:8d}     {:8d}     {:8d}
-----------------------------------------------------------")

(defn cloc-format [counts]
  (let [files (-get counts "file" 0)
        code (-get counts "code"0)
        comments (-get counts "comment" 0)
        blanks (-get counts "blank" 0)
        lines (+ code comments blanks)]
    (s.format cloc-template files lines code comments blanks)))

(defmain []
  (let [args (parse-args [["-v" "--verbose" :action "store_true"]
                          ["-l" "--lang" :default "cpp"]
                          ["-T" "--threads" :type int]
                          ["src" :nargs "+"]])
        logger (when args.verbose (cloc-get-logger))
        counter (SrcCounter.from-language args.lang)
        counts (if args.threads
                   (counter.multi-count-src args.src :n args.threads :logger logger)
                   (counter.count-src args.src :logger logger))]
    (print (cloc-format counts))))

(export
  :objects [SrcCounter])
