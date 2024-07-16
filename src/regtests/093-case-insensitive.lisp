;;; test for the (new) case-insensitivity of lyk

(require 'regtests)

(test "case-i symbols a" (eq 'fall 'FALL))
(test "case-i symbols b" (eq 'Fall 'fall))
(test "case-i symbols c" (eq 'Fall 'FALL))

(test "case-i form l-u"
      (let ((dotimes1-l
             "(defmacro dotimes1 (countargs &rest bodyforms)
  \"(dotimes1 (var count-form &optional result-form) &rest bodyforms)
dotimes evaluates count-form, which should produce an integer. If
count-form is zero or negative, the body is not executed. dotimes then
executes the body once for each integer from 1 up to including the
value of count-form, in the order in which the statements occur, with
var bound to each integer. Then result-form is evaluated. At the time
result-form is processed, var is bound to the number of times the body
was executed.\"
  (let ((var (car countargs))
        (endval (cadr countargs))
        (resultform (caddr countargs)))
    (let ((end (gensym)))
      `(let ((,var 1)
             (,end ,endval))
         (while (<= ,var ,end)
           ,@bodyforms
           (incf ,var))
         ,resultform))))")
            (dotimes1-u
             "(DEFMACRO DOTIMES1 (COUNTARGS &REST BODYFORMS)
  \"(dotimes1 (var count-form &optional result-form) &rest bodyforms)
dotimes evaluates count-form, which should produce an integer. If
count-form is zero or negative, the body is not executed. dotimes then
executes the body once for each integer from 1 up to including the
value of count-form, in the order in which the statements occur, with
var bound to each integer. Then result-form is evaluated. At the time
result-form is processed, var is bound to the number of times the body
was executed.\"
  (LET ((VAR (CAR COUNTARGS))
        (ENDVAL (CADR COUNTARGS))
        (RESULTFORM (CADDR COUNTARGS)))
    (LET ((END (GENSYM)))
      `(LET ((,VAR 1)
             (,END ,ENDVAL))
         (WHILE (<= ,VAR ,END)
           ,@BODYFORMS
           (INCF ,VAR))
         ,RESULTFORM))))"))
        (equal (read dotimes1-l) (read dotimes1-u))))

(test "case-i form l-c"
      (let ((dotimes1-l
             "(defmacro dotimes1 (countargs &rest bodyforms)
  \"(dotimes1 (var count-form &optional result-form) &rest bodyforms)
dotimes evaluates count-form, which should produce an integer. If
count-form is zero or negative, the body is not executed. dotimes then
executes the body once for each integer from 1 up to including the
value of count-form, in the order in which the statements occur, with
var bound to each integer. Then result-form is evaluated. At the time
result-form is processed, var is bound to the number of times the body
was executed.\"
  (let ((var (car countargs))
        (endval (cadr countargs))
        (resultform (caddr countargs)))
    (let ((end (gensym)))
      `(let ((,var 1)
             (,end ,endval))
         (while (<= ,var ,end)
           ,@bodyforms
           (incf ,var))
         ,resultform))))")
            (dotimes1-c
             "(Defmacro Dotimes1 (Countargs &Rest Bodyforms)
  \"(dotimes1 (var count-form &optional result-form) &rest bodyforms)
dotimes evaluates count-form, which should produce an integer. If
count-form is zero or negative, the body is not executed. dotimes then
executes the body once for each integer from 1 up to including the
value of count-form, in the order in which the statements occur, with
var bound to each integer. Then result-form is evaluated. At the time
result-form is processed, var is bound to the number of times the body
was executed.\"
  (Let ((Var (Car Countargs))
        (Endval (Cadr Countargs))
        (Resultform (Caddr Countargs)))
    (Let ((End (Gensym)))
      `(Let ((,Var 1)
             (,End ,Endval))
         (While (<= ,Var ,End)
           ,@Bodyforms
           (Incf ,Var))
         ,Resultform))))"))
        (Equal (Read Dotimes1-L) (Read Dotimes1-C))))

(done-testing)
