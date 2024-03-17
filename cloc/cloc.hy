(require
  dash *)

(import
  dash *
  dash.strtools :as s
  cloc.counter [SrcCounter])

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

(defn cloc-main []
  (let [args (parse-args [["-v" "--verbose" :action "store_true"]
                          ["-l" "--lang" :default "c"]
                          ["--ignore-errors" :action "store_true"]
                          ["-T" "--threads" :type int]
                          ["src" :nargs "+"]])
        counter (SrcCounter.from-language args.lang)
        kwargs {"logger" (when args.verbose (cloc-get-logger))
                "ignore_errors" args.ignore-errors}
        counts (if args.threads
                   (counter.multi-count-src args.src :n args.threads #** kwargs)
                   (counter.count-src args.src #** kwargs))]
    (print (cloc-format counts))))

(defmain [] (cloc-main))
