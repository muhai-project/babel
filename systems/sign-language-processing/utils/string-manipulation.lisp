(in-package :slp)

(defun escape-colons (string)
  "escapes any colons in string by placing \\ in front of them"
  (string-replace string ":" "\\:"))

(defun replace-round-brackets (string)
  "replaces any occurrence of round opening or closing brackets
   in string with the equivalent square bracket"
  (string-replace
   (string-replace
    string
    "(" "[")
   ")" "]"))

(defun replace-spaces (string)
  "replaces all occurrences of spaces in the string with dashes"
  (string-replace string " " "-"))

(defun get-gloss-prefix (gloss)
  "returns the prefix of a gloss"
  (read-from-string
   (first
   (split-sequence:split-sequence
    #\:
    (string gloss)))))

(defun string->predicates (string)
  "transforms a string of predicates into a list of predicates. It escapes colons."
  (let ((split-string
         (split-sequence::split-sequence
          #\)
          string ))
        (output-list '()))
    (delete
     ""
     split-string
     :test #'string=)
    (loop for item in split-string
          for sublist = '()
          for cleaned-item = (escape-colons item)
          for split-predicate =
            (delete
             ""
             (split-sequence::split-sequence
              #\(
              cleaned-item)
             :test #'string=)
          for first-arg =
            (remove
             #\,
             (first split-predicate))
          for remaining-args =
            (split-sequence::split-sequence
             #\,
             (second split-predicate))
          do (push
              (read-from-string
               first-arg
               :geoquery-lsfb)
              sublist)
             (loop for remaining-arg in remaining-args
                   do (push
                       (read-from-string
                        remaining-arg)
                       sublist))
             (push
              (reverse sublist)
              output-list))
    (reverse output-list)))