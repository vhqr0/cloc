(require
  dash *)

(import
  dash *
  dash.strtools :as s
  collections [Counter])

(defn lisp-line-type [line]
  (let [line (s.strip line)]
    (cond (s.blank? line) "blank"
          (s.starts-with? line ";") "comment"
          True "code")))

(defn lisp-line-count [code]
  (Counter (-map lisp-line-type code)))

(export
  :objects [lisp-line-count])
