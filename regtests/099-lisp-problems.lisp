;;; Solutions to (some of) the Ninety-Nine Lisp Problems
;;; http://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/L-99_Ninety-Nine_Lisp_Problems.html

(require 'regtests)

;; P01 (*) Find the last box of a list.
;; Example:
;; * (my-last '(a b c d))
;; (D)

(defun my-last (l)
  (if (null (cdr l))
      l
      (my-last (cdr l))))

(test-is "lisp problem 01/99" (my-last '(a b c d)) '(d))

;; P02 (*) Find the last but one box of a list.
;; Example:
;; * (my-but-last '(a b c d))
;; (C D)

(defun my-but-last (l)
  (if (null (cddr l))
      l
      (my-but-last (cdr l))))

(test-is "lisp problem 02/99" (my-but-last '(a b c d)) '(c d))

;; P03 (*) Find the K'th element of a list.
;; The first element in the list is number 1.
;; Example:
;; * (element-at '(a b c d e) 3)
;; C

(defun element-at (l n)
  (if (<= n 1)
      (car l)
      (element-at (cdr l) (1- n))))

(test-is "lisp problem 03/99" (element-at '(a b c d e) 3) 'c)

;; P04 (*) Find the number of elements of a list.

(defun my-len (l)
  (if (null l)
      0
      (1+ (my-len (cdr l)))))

(test-is "lisp problem 04/99" (my-len '(a b c d e f g)) 7)

;; P05 (*) Reverse a list.

(defun my-reverse (l)
  (if (null l)
      l
      (append (my-reverse (cdr l)) (list (car l)))))

(test-is "lisp problem 05/99" (my-reverse '(a b c d e f g))
         '(g f e d c b a))

;; P06 (*) Find out whether a list is a palindrome.
;; A palindrome can be read forward or backward; e.g. (x a m a x).

(defun equal-list (l1 l2)
  (if (and (null l1) (null l2))
      t
      (if (eq (car l1) (car l2))
          (equal-list (cdr l1) (cdr l2))
          nil)))

(defun is-palindrome (l)
  (equal-list l (my-reverse l)))

(test-is "lisp problem 06/99a" (is-palindrome '(a b c d e b a)) nil)
(test-is "lisp problem 06/99b" (is-palindrome '(a b c d c b a)) t)

;; P07 (**) Flatten a nested list structure.
;; Transform a list, possibly holding lists as elements into a `flat'
;; list by replacing each list with its elements (recursively).

;; Example:
;; * (my-flatten '(a (b (c d) e)))
;; (A B C D E)

;; Hint: Use the predefined functions list and append.

(defun my-flatten (l)
  (if (null l)
      l
      (if (atom l)
          (list l)
        (append (my-flatten (car l)) (my-flatten (cdr l))))))

(test-is "lisp problem 07/99" (my-flatten '(a (b (c d) e))) '(a b c d e))


;; P08 (**) Eliminate consecutive duplicates of list elements.
;; If a list contains repeated elements they should be replaced with a
;; single copy of the element. The order of the elements should not be
;; changed.

;; Example:
;; * (compress '(a a a a b c c a a d e e e e))
;; (A B C A D E)

(defun compress (l)
  (if (null (cdr l))
      l
      (if (eq (car l) (cadr l))
          (compress (cdr l))
          (cons (car l) (compress (cdr l))))))

(test-is "lisp problem 08/99" (compress '(a a a a b c c a a d e e e e))
         '(a b c a d e))


;; P09 (**) Pack consecutive duplicates of list elements into sublists.
;; If a list contains repeated elements they should be placed in
;; separate sublists.

;; Example:
;; * (pack '(a a a a b c c a a d e e e e))
;; ((A A A A) (B) (C C) (A A) (D) (E E E E))

(defun pack (l)
  (pack-helper l nil '()))

(defun pack-helper (l curel accu)
  (if (null l)
      (reverse (cons curel accu))
    (let ((first (car l)))
      (if curel
          (if (eq first (car curel))
              (pack-helper (cdr l) (cons first curel) accu)
          (pack-helper (cdr l) (list first) (cons curel accu)))
        (pack-helper (cdr l) (list first) accu)))))

(test-is "lisp problem 09/99" (pack '(a a a a b c c a a d e e e e))
         '((a a a a) (b) (c c) (a a) (d) (e e e e)))

;; P10 (*) Run-length encoding of a list.
;; Use the result of problem P09 to implement the so-called run-length
;; encoding data compression method. Consecutive duplicates of
;; elements are encoded as lists (N E) where N is the number of
;; duplicates of the element E.

;; Example:
;; * (encode '(a a a a b c c a a d e e e e))
;; ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))

(defun encode (l)
  (encode-helper (pack l)))

(defun encode-helper (l)
  (if (null l)
      l
    (let (((first . rest) l))
      (cons (list (length first) (car first))
            (encode-helper rest)))))

(test-is "lisp problem 10/99" (encode '(a a a a b c c a a d e e e e))
         '((4 a) (1 b) (2 c) (2 a) (1 d)(4 e)))

;; P11 (*) Modified run-length encoding.
;; Modify the result of problem P10 in such a way that if an element
;; has no duplicates it is simply copied into the result list. Only
;; elements with duplicates are transferred as (N E) lists.

;; Example:
;; * (encode-modified '(a a a a b c c a a d e e e e))
;; ((4 A) B (2 C) (2 A) D (4 E))

(defun encode-modified (l)
  (encode-modified-helper (pack l)))

(defun encode-modified-helper (l)
  (if (null l)
      l
    (let (((first . rest) l))
      (cons (let ((flen (length first))
                  (elem (car first)))
              (if (= flen 1)
                  elem
                (list flen elem)))
            (encode-modified-helper rest)))))

(test-is "lisp problem 11/99" (encode-modified '(a a a a b c c a a d e e e e))
         '((4 a) b (2 c) (2 a) d (4 e)))

;; P12 (**) Decode a run-length encoded list.
;; Given a run-length code list generated as specified in problem P11.
;; Construct its uncompressed version.

(defun decode (l)
  (if (null l)
      l
      (let (((elem . rest) l))
        (if (consp elem)
            (let (((len sym) elem))
              (cons sym (decode (if (> len 1)
                                    (cons (list (1- len) sym) rest)
                                  rest))))
          (cons elem (decode rest))))))

(test-is "lisp problem 12/99" (decode '((4 a) b (2 c) (2 a) d (4 e)))
         '(a a a a b c c a a d e e e e))

;; P13 (**) Run-length encoding of a list (direct solution).
;; Implement the so-called run-length encoding data compression method
;; directly. I.e. don't explicitly create the sublists containing the
;; duplicates, as in problem P09, but only count them. As in problem
;; P11, simplify the result list by replacing the singleton lists (1
;; X) by X.

;; Example:
;; * (encode-direct '(a a a a b c c a a d e e e e))
;; ((4 A) B (2 C) (2 A) D (4 E))

(defun encode-direct (l)
  (encode-direct-helper l '() '()))

(defun encode-direct-helper (l curel accu)
  (if (null l)
      (reverse (if curel
                   (cons curel accu)
                 accu))
    (let (((first . rest) l))
      (if curel
          (let (((count elem) curel))
            (if (eq first elem)
                (encode-direct-helper rest (list (1+ count) elem) accu)
              (encode-direct-helper rest
                                    (list 1 first)
                                    (cons (if (= count 1)
                                              elem
                                            curel) accu))))
        (encode-direct-helper rest (list 1 first) accu)))))

(test-is "lisp problem 13/99" (encode-direct '(a a a a b c c a a d e e e e))
         '((4 a) b (2 c) (2 a) d (4 e)))

;; P14 (*) Duplicate the elements of a list.
;; Example:
;; * (dupli '(a b c c d))
;; (A A B B C C C C D D)

(defun dupli (l)
  (if (null l)
      l
    (let (((elem . rest) l))
      (cons elem (cons elem (dupli rest))))))

(test-is "lisp problem 14/99" (dupli '(a b c c d))
         '(a a b b c c c c d d))

;; P15 (**) Replicate the elements of a list a given number of times.
;; Example:
;; * (repli '(a b c) 3)
;; (A A A B B B C C C)

(defun repli (l n)
  (my-flatten (map #'(lambda (el) (make-list n el)) l)))

(test-is "lisp problem 15/99" (repli '(a b c) 3)
         '(a a a b b b c c c))


;; P16 (**) Drop every N'th element from a list.
;; Example:
;; * (drop '(a b c d e f g h i k) 3)
;; (A B D E G H K)

(defun drop (l n)
  (flet ((helper (l i)
           (if (null l)
               nil
               (if (= i 1)
                   (helper (cdr l) n)
                   (cons (car l) (helper (cdr l) (1- i)))))))
    (helper l n)))

(test-is "lisp problem 16/99" (drop '(a b c d e f g h i k) 3)
         '(a b d e g h k))

;; P17 (*) Split a list into two parts; the length of the first part is given.
;; Do not use any predefined predicates.

;; Example:
;; * (split '(a b c d e f g h i k) 3)
;; ( (A B C) (D E F G H I K))

(defun split (l n)
  (flet ((helper (l n acc)
                   (if (zerop n)
                       (list (reverse acc) l)
                     (helper (cdr l) (1- n) (cons (car l) acc)))))
    (helper l n nil)))

(test-is "lisp problem 17/99" (split '(a b c d e f g h i k) 3)
         '((a b c) (d e f g h i k)))
  

;; P18 (**) Extract a slice from a list.
;; Given two indices, I and K, the slice is the list containing the
;; elements between the I'th and K'th element of the original list (both
;; limits included). Start counting the elements with 1.
;; Example:
;; * (slice '(a b c d e f g h i k) 3 7)
;; (C D E F G)

(defun slice (l start end)
  (if (zerop end)
      nil
    (if (<= start 1)
        (cons (car l) (slice (cdr l) 0 (1- end)))
      (slice (cdr l) (1- start) (1- end)))))

(test-is "lisp problem 18/99" (slice '(a b c d e f g h i k) 3 7)
         '(c d e f g))

;; P19 (**) Rotate a list N places to the left.
;; Examples:
;; * (rotate '(a b c d e f g h) 3)
;; (D E F G H A B C)

;; * (rotate '(a b c d e f g h) -2)
;; (G H A B C D E F)

;; Hint: Use the predefined functions length and append, as well as the
;; result of problem P17.

(defun rotate (l n)
  (if (< n 0)
      (setf n (+ n (length l))))
  (let (((a b) (split l n)))
    (append b a)))

(test-is "lisp problem 19/99a" (rotate '(a b c d e f g h) 3)
         '(d e f g h a b c))
(test-is "lisp problem 19/99b" (rotate '(a b c d e f g h) -2)
         '(g h a b c d e f))

;; P20 (*) Remove the K'th element from a list.
;; Example:
;; * (remove-at '(a b c d) 2)
;; (A C D)

(defun remove-at (l n)
  (if (= 1 n)
      (cdr l)
    (cons (car l) (remove-at (cdr l) (1- n)))))

(test-is "lisp problem 20/99" (remove-at '(a b c d) 2)
         '(a c d))

; P21 (*) Insert an element at a given position into a list.
;; Example:
;; * (insert-at 'alfa '(a b c d) 2)
;; (A ALFA B C D)

(defun insert-at (elem l n)
  (if (= 1 n)
      (cons elem l)
    (cons (car l) (insert-at elem (cdr l) (1- n)))))

(test-is "lisp problem 21/99" (insert-at 'alfa '(a b c d) 2)
         '(a alfa b c d))

;; P22 (*) Create a list containing all integers within a given range.
;; If first argument is smaller than second, produce a list in decreasing order.
;; Example:
;; * (range 4 9)
;; (4 5 6 7 8 9)

(defun range (start end)
  (if (> start end)
      nil
    (cons start (range (1+ start) end))))

(test-is "lisp problem 22/99" (range 4 9) '(4 5 6 7 8 9))

;; P23 (**) Extract a given number of randomly selected elements from a list.
;; The selected items shall be returned in a list.
;; Example:
;; * (rnd-select '(a b c d e f g h) 3)
;; (E D A)

;; Hint: Use the built-in random number generator and the result of
;; problem P20.

(defun rnd-select (l n)
  (if (zerop n)
      nil
    (let ((rnd (random (length l) t)))
      (cons (nth rnd l) (rnd-select (remove-at l (1+ rnd)) (1- n))))))

(test-is "lisp problem 23/99" (let ((result (rnd-select '(2 3 4 5 6 7 8 9) 3)))
                                (and (= 3 (length result))
                                     (apply #'/= result)))
         t)

;; P24 (*) Lotto: Draw N different random numbers from the set 1..M.
;; The selected numbers shall be returned in a list.
;; Example:
;; * (lotto-select 6 49)
;; (23 1 17 33 21 37)

;; Hint: Combine the solutions of problems P22 and P23.

(defun lotto-select (n to)
  (sort (rnd-select (range 1 to) n) #'<))

(test-is "lisp problem 24/99" (let ((result (lotto-select 6 49)))
                                (and (= 6 (length result))
                                     (apply #'/= result)))
         t)

;; P25 (*) Generate a random permutation of the elements of a list.
;; Example:
;; * (rnd-permu '(a b c d e f))
;; (B A D C E F)

;; Hint: Use the solution of problem P23.

(defun rnd-permu (l)
  (rnd-select l (length l)))

(test-is "lisp problem 25/99" (let ((result (rnd-permu '(a b c d e f))))
                                (and (= 6 (length result))
                                     (apply #'/= result)))
         t)

;; P26 (**) Generate the combinations of K distinct objects chosen
;; from the N elements of a list
;; In how many ways can a committee of 3 be chosen from a group of 12
;; people? We all know that there are C(12,3) = 220 possibilities
;; (C(N,K) denotes the well-known binomial coefficients). For pure
;; mathematicians, this result may be great. But we want to really
;; generate all the possibilities in a list.

;; Example:
;; * (combination 3 '(a b c d e f))
;; ((A B C) (A B D) (A B E) ... )

(defun combination (n l)
  "Return all combinations of N elements of list L, leftmost first.
If N ist smaller than the length of L, return L."
  (let ((len (length l)))
    (if (<= len n)
        (list l)
      (if (zerop n)
          (list nil)
        (let (((first . rest) l))
          (nconc (map (lambda (lrest) (cons first lrest))
                      (combination (1- n) rest))
                 (combination n rest)))))))

(test-is "lisp problem 26/99" (combination 3 '(a b c d e f))
         '((a b c) (a b d) (a b e) (a b f)
           (a c d) (a c e) (a c f)
           (a d e) (a d f)
           (a e f)
           (b c d) (b c e) (b c f)
           (b d e) (b d f)
           (b e f)
           (c d e) (c d f)
           (c e f)
           (d e f)))

;; P27 (**) Group the elements of a set into disjoint subsets.
;; a) In how many ways can a group of 9 people work in 3 disjoint
;;    subgroups of 2, 3 and 4 persons? Write a function that generates
;;    all the possibilities and returns them in a list.
;; Example:
;; * (group3 '(aldo beat carla david evi flip gary hugo ida))
;; ( ( (ALDO BEAT) (CARLA DAVID EVI) (FLIP GARY HUGO IDA) )
;; ... )
;;
;; b) Generalize the above predicate in a way that we can specify a
;;    list of group sizes and the predicate will return a list of
;;    groups.
;; Example:
;; * (group '(aldo beat carla david evi flip gary hugo ida) '(2 2 5))
;; ( ( (ALDO BEAT) (CARLA DAVID) (EVI FLIP GARY HUGO IDA) )
;; ... )
;;
;; Note that we do not want permutations of the group members; i.e.
;; ((ALDO BEAT) ...) is the same solution as ((BEAT ALDO) ...).
;; However, we make a difference between ((ALDO BEAT) (CARLA DAVID)
;; ...) and ((CARLA DAVID) (ALDO BEAT) ...).
;;
;; You may find more about this combinatorial problem in a good book
;; on discrete mathematics under the term "multinomial coefficients".

;; idea (numbers for a): take a 2 combination, then the 3 combinations
;; of the rest of the list etc. Sounds relatively easy. Although a bit
;; computation-intense, to say the least.

(defun list-subtract (l1 l2)
  "Subtract all members of list L2 from the list L1.
Return the remaining elements of L1 in order."
  (if (null l1)
      nil
    (let (((first . rest) l1))
      (if (member first l2)
          (list-subtract rest l2)
        (cons first (list-subtract rest l2))))))

(defun group3 (set)
  (group set '(2 3 4)))

(defun group (set numbers)
  (if (null numbers)
      nil
    (let ((this-level-len (car numbers)))
      (if (<= (length set) this-level-len)
          (list set)
        (let ((combinations (combination this-level-len set))
              result)
          (debug-vars combinations)
          (dolist (combi combinations (nreverse result))
            (debug-vars combi)
            (let ((nextlevel-groups
                   (group (list-subtract set combi) (cdr numbers))))
              (debug-vars nextlevel-groups)
              (dolist (nextlevel nextlevel-groups)
                (debug-vars nextlevel)
                (let ((newelem (list combi nextlevel)))
                  (debug-vars newelem)
                  (push newelem result))))))))))

(done-testing)
