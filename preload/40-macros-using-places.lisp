;; these must come after the places

(defmacro for (params &rest bodyforms)
  "(for (var from to [step [test]])
The for loop uses `var' as the counter variable, starting with `from',
adding `step' to `var' after each run, ending when `(test var to)' no
longer is true. The default step is 1; the default test is #'<."
  (let (((var from to step test) params))
    (setq step (or step 1))
    (setq test (or test #'<))
    `(let ((,var ,from))
       (while (,test ,var ,to)
         ,@bodyforms
         (incf ,var ,step)))))

(defmacro with-gensyms (syms &rest body)
  "Run the BODY with the symbols in SYMS (a list) bound to gensyms.
This is meant to simplify macro definitions that would otherwise
use a
  (let ((param1 (gensym))
        (param2 (gensym)))
        ... )
    ,@body)
symbol definition chain explicitly."
  (let (decls)
    (while syms
      (push (list (pop syms) '(gensym)) decls))
    `(let ,decls
       ,@body)))

