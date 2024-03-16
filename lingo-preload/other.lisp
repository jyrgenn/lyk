;; smallish things that don't belong elsewhere

(defun arg-number-is-one (arg)
  "Iff ARG (or the number of its elements) is 1, return true."
  (let ((n (cond ((numberp arg) arg)
                 ((sequencep arg) (length arg)))))
    (= n 1)))
  
(defun plural-s (arg)
  "Return a plural-s or an empty string as appropriate for ARG.
If ARG is a number, return \"s\" if it is zero or greater than 1, but
an empty string if it is 1.
If ARG is a sequence, do the same for the number of it elements."
  (if (arg-number-is-one arg)
      ""
    "s"))

(defun plural-ies (arg)
  "Return a plural \"ies\" or singular \"y\" as appropriate for ARG.
If ARG is a number, return \"ies\" if it is zero or greater than 1, but
\"y\" if it is 1.
If ARG is a sequence, do the same according to the number of it elements."
  (if (arg-number-is-one arg)
      "y"
    "ies"))

(defun function-symbols ()
  "Return a list of all function symbols."
  (filter (λ (sym) (get sym 'func-defined-in)) (sys:symbols)))

