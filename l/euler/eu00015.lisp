;;; Project Euler problem 15

;; 1
;; _ _ _
;;      |
;;      |
;;      |
;; lllrrr

;; 2
;;  _ _  
;;     |_ 
;;       |
;;       |
;; llrlrr

;; 3
;;  _ _  
;;     |  
;;     |_ 
;;       |
;; llrrlr

;; 4
;;  _ _  
;;     |  
;;     |  
;;     |_
;; llrrrl

;; 5
;;  _    
;;   |_ _ 
;;       |
;;       |
;; lrllrr

;; 6
;;  _    
;;   |_   
;;     |_ 
;;       |
;; lrlrlr

;; 7
;;  _    
;;   |_   
;;     |  
;;     |_
;; lrlrrl

;; 8
;;  _    
;;   |    
;;   |_ _ 
;;       |
;; lrrllr

;; 9
;;  _    
;;   |    
;;   |_   
;;     |_ 
;; lrrlrl

;; 10
;;  _    
;;   |    
;;   |    
;;   |_ _
;; lrrrll

;; 11
;;
;; |_ _ _ 
;;       |
;;       |
;; rlllrr

;; 12
;;      
;; |_ _   
;;     |_ 
;;       |
;; rllrlr

;; 13
;;      
;; |_ _   
;;     |  
;;     |_
;; rllrrl

;; 14
;;      
;; |_     
;;   |_ _ 
;;       |
;; rlrllr

;; 15
;;      
;; |_     
;;   |_   
;;     |_
;; rlrlrl

;; 16
;;      
;; |_     
;;   |    
;;   |_ _
;; rlrrll

;; 17
;;      
;; |      
;; |_ _ _
;;       |
;; rrlrll

;; 18
;;      
;; |      
;; |_ _   
;;     |_
;; rrrlll

;; 19
;;      
;; |      
;; |_    
;;   |_ _
;; rrlrll

;; 20
;;      
;; |      
;; |      
;; |_ _ _
;; rrrlll

(defun faculty (n)
  (if (< n 2)
      1
    (* n (faculty-r (1- n)))))

(defun eu15 ()
  (let ((f20 (faculty 20)))
    (/ (faculty 40) (* f20 f20))))

;; result 137846528820
