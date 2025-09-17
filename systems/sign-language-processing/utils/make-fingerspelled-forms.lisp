(in-package :slp)

(defparameter *fingerspelling-alphabet*
  '((:a "")
    (:b "")
    (:c "")
    (:d "")
    (:e "")
    (:f "")
    (:g "")
    (:h "")
    (:i "")
    (:j "")
    (:k "")
    (:l "")
    (:m "")
    (:n "")
    (:o "")
    (:p "")
    (:q "")
    (:r "")
    (:s "")
    (:t "")
    (:u "")
    (:v "")
    (:w "")
    (:x "")
    (:y "")
    (:z "")))

(defun make-fingerspelling (string)
  "takes a string as input (roman alphabet) and returns another string (fingerspelled form in hamnosys)"
  (loop with output = ""

        ;; find the hamnosys character for each character of the input
        for character across string
        for character-hamnosys =
          (first (cdr (assoc (read-from-string
                       (format nil ":~a" character))
                      *fingerspelling-alphabet*)))
        ;; if the character is not the first in the string, add a comma between the previous and this character
        do (if (string=
                output
                "")
             (setf
              output
              (concatenate
               'string
               output
               character-hamnosys))
             (setf
              output
              (concatenate
               'string
               output
               (string
                (code-char  57514))
               character-hamnosys)))
        finally (return output)))