(require
  dash *)

(import
  dash *
  dash.strtools :as s
  enum)

;; 0:open f:close e:escape
(defclass State [enum.Enum]
  (setv normal     (enum.auto)   ; _
        normal-e   (enum.auto)   ; \_
        comment-0  (enum.auto)   ; /_
        comment-f  (enum.auto)   ; /**_
        comment-1  (enum.auto)   ; //_
        comment-2  (enum.auto)   ; /*_*/
        comment-e1 (enum.auto)   ; //\_
        comment-e2 (enum.auto)   ; /*\_*/
        string-0   (enum.auto)   ; R_
        string-00  (enum.auto)   ; R"_
        string-f   (enum.auto)   ; R"()_
        string-1   (enum.auto)   ; "_"
        string-2   (enum.auto)   ; R"(_)"
        string-e1  (enum.auto)   ; "\_"
        string-e2  (enum.auto)   ; R"(\_)"
        char-f     (enum.auto)   ; '?_'
        char       (enum.auto)   ; '_'
        char-e     (enum.auto)   ; '\_'
        ))

(defn enumerate-code [code]
  (->> (gfor #(row line) (enumerate code)
             (gfor #(col char) (enumerate line)
                   #(row col char)))
       (-concat-in)))

(defclass CCParser []
  (defn __init__ [self]
    (setv self.state State.normal
          self.start-point False
          self.comment-regions (list)))

  (defn parse [self code]
    (-each (enumerate-code code) self.input)
    (unless (= self.state State.normal)
      (raise (RuntimeError (s.format "parse error: end of file at {}" self.state)))))

  (defn input [self it]
    (let [#(row col char) it]
      (match self.state
             State.normal (match char
                                 "\\" (setv self.state State.normal-e)
                                 "/"  (setv self.state State.comment-0)
                                 "R"  (setv self.state State.string-0)
                                 "\"" (setv self.state State.string-1)
                                 "'"  (setv self.state State.char))
             State.normal-e (setv self.state State.normal)
             State.comment-0 (match char
                                    "\\" (setv self.state State.normal-e)
                                    "/"  (setv self.state State.comment-1
                                               self.start-point #(row (dec col)))
                                    "*"  (setv self.state State.comment-2
                                               self.start-point #(row (dec col)))
                                    "R"  (setv self.state State.string-0)
                                    "\"" (setv self.state State.string-1)
                                    _    (setv self.state State.normal))
             State.comment-f (match char
                                    "\\" (setv self.state State.comment-e2)
                                    "/"  (do
                                           (.append self.comment-regions #(self.start-point #(row (inc col))))
                                           (setv self.state State.normal
                                                 self.start-point None))
                                    _ (setv self.state State.comment-2))
             State.comment-1 (match char
                                    "\\" (setv self.state State.comment-e1)
                                    "\n" (do
                                           (.append self.comment-regions #(self.start-point #(row (inc col))))
                                           (setv self.state State.normal
                                                 self.start-point None)))
             State.comment-2 (match char
                                    "\\" (setv self.state State.comment-e2)
                                    "*"  (setv self.state State.comment-f))
             State.comment-e1 (setv self.state State.comment-1)
             State.comment-e2 (setv self.state State.comment-2)
             State.string-0 (match char
                                   "\\" (setv self.state State.normal-e)
                                   "\"" (setv self.state State.string-00)
                                   _ (setv self.state State.normal))
             State.string-00 (match char
                                    "(" (setv self.state State.string-2)
                                    _ (raise (RuntimeError (s.format "parse error: cannot find ( after R\" at {}:{}" row col))))
             State.string-f (match char
                                   "\\" (setv self.state State.string-e2)
                                   "\"" (setv self.state State.normal)
                                   _    (setv self.state State.string-2))
             State.string-1 (match char
                                   "\\" (setv self.state State.string-e1)
                                   "\"" (setv self.state State.normal)
                                   "\n" (raise (RuntimeError (s.format "parse error: find newline in string at {}:{}" row col))))
             State.string-2 (match char
                                   "\\" (setv self.state State.string-e2)
                                   ")"  (setv self.state State.string-f))
             State.string-e1 (setv self.state State.string-1)
             State.string-e2 (setv self.state State.string-2)
             State.char-f (match char
                                 "'" (setv self.state State.normal)
                                 _  (raise (RuntimeError (s.format "parse error: find open char at {}:{}" row col))))
             State.char (match char
                               "'"  (raise (RuntimeError (s.format "parse error: find empty char at {}:{}" row col)))
                               "\\" (setv self.state State.char-e)
                               _    (setv self.state State.char-f))
             State.char-e (setv self.state State.char-f)
             _ (raise (RuntimeError (s.format "parse error: unknown state {}" self.state)))))))

(export
  :objects [CCParser])
