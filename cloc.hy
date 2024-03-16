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
    (-> (s.decode code (if (none? encoding) "utf-8" encoding))
        ;; TODO: update dash.strtools.split-lines add :keepends kwarg
        ;; (s.split-lines :keepends True)
        (.splitlines :keepends True))))

(defn parse-code [parser #^ (py "list[str]") code]
  "Parse code (list of lines) to AST Tree."
  (let [readfn (fn [_ point]
                 (let [#(row col) point]
                   (when (< row (len code))
                     (let [line (get code row)]
                       (when (< col (len line))
                         (s.encode (cut line col None)))))))]
    (.parse parser readfn)))

(defn tree-walk [walkfn tree]
  (defn walk [walkfn node]
    (walkfn node)
    (--each node.children (walk walkfn it)))
  (walk walkfn tree.root-node))

(defn #^ (py "list[list[tuple[int, int]]]") get-regions [tree types #^ (py "list[str]") code]
  "Walk AST tree, collect regions (list of start/end) of each lines by type."
  (let [comment-regions (list (-repeatedly-n (len code) list))
        walkfn (fn [node]
                 (when (in node.type types)
                   (let [#(#(start-row start-col) #(end-row end-col)) #(node.start-point node.end-point)]
                     (while (< start-row end-row)
                       (let [end-col (len (get code start-row))]
                         (when (< start-col end-col)
                           (-update! comment-regions start-row -conj! #(start-col end-col))))
                       (+= start-row 1)
                       (setv start-col 0))
                     (when (< start-col end-col)
                       (-update! comment-regions start-row -conj! #(start-col end-col))))))]
    (tree-walk walkfn tree)
    comment-regions))

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

(defn path-iter [path]
  (when (str? path)
    (setv path (Path path)))
  (cond (.is-dir path) (yield-from
                         (--mapcat
                           (let [#(base dirs files) it]
                             (--map (/ base it) files))
                           (.walk path)))
        (.is-file path) (yield path)))

(defn src-iter [paths extensions]
  "Iter source files from paths by extension."
  (->> (-map path-iter paths)
       -concat-in
       (--filter (in it.suffix extensions))))



(defn get-parser [#^ str language]
  (import tree-sitter-languages [get-parser])
  (get-parser language))

(defclass SrcCounter []
  (setv src-extensions None
        ts-language None
        ts-comment-types #("comment"))

  (setv override-extensions None
        override-languages None)

  (setv extension-dict (dict)
        language-dict (dict))

  (defn __init-subclass__ [cls #* args #** kwargs]
    "Register extensions/languages to dict when create subclass.
Respect override-extensions/languages."
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

  (defn count-code [self #^ bytes code]
    (let [code (decode-code code)
          comment-regions (-> (get-parser self.ts-language)
                              (parse-code code)
                              (get-regions self.ts-comment-types code))]
      (Counter (--map
                 (let [#(line comment-region) it]
                   (cond (s.blank? (s.strip line)) "blank"
                         (and comment-region (s.blank? (s.strip (delete-region line comment-region)))) "comment"
                         True "code"))
                 (-zip code comment-regions)))))

  (defn count-file [self path]
    (if (> (. (.stat path) st-size) self.buffer-size)
        (raise (RuntimeError "file too large"))
        (let [code (.read (.open path "rb"))]
          (.count-code self code))))

  (defn count-src [self paths [logger None]]
    (->> (src-iter paths self.src-extensions)
         (--keep
           (try
             (when logger
               (.info logger "count %s" it))
             (.count-file self it)
             (except [e Exception]
               (when logger
                 (.info logger "except while counting %s %s" it e))
               None)))
         (--reduce-from
           (-merge-with o.add acc it {"file" 1})
           (dict))))

  (defn multi-count-src [self paths n [logger None]]
    (import multiprocessing)

    (setv tasks (multiprocessing.Queue (* 2 n))
          results (multiprocessing.Queue))

    (defn counter []
      (let [acc (dict)]
        (while True
          (let [path (.get tasks)]
            (if (none? path)
                (break)
                (do
                  (when logger
                    (.info logger "count %s" path))
                  (try
                    (let [it (.count-file self path)]
                      (setv acc (-merge-with o.add acc it {"file" 1})))
                    (except [e Exception]
                      (when logger
                        (.info logger "except while counting %s %s" path e))))))))
        (.put results acc)))

    (setv threads (list (--repeatedly-n n (multiprocessing.Process :target counter))))
    (--each threads (.start it))
    (--each (src-iter paths self.src-extensions) (.put tasks it))
    (--dotimes n (.put tasks None))
    (--each threads (.join it))
    (-merge-with o.add #* (--repeatedly-n n (.get-nowait results)))))



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
