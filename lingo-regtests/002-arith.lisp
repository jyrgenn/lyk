(require 'regtests)

(test-is "- 1" (- 3) -3)
(test-is "- 2" (- 3 2) 1)
(test-is "- 3" (- 13 4 5) 4)
(test-is "/ 1a" (/ 1) 1)
(test-is "/ 1b" (/ 2.0) 0.5)
(test-is "/ 2" (/ 6 5.0) 1.2)
(test-is "/ 3" (/ 12 2 3) 2)
(test-is "/ 4" (/ 64 2 2 2) 8)
(test-is "+ 0" (+) 0)
(test-is "+ 1" (+ 3.3) 3.3)
(test-is "+ 2" (+ 2 3) 5)
(test-is "+ 2a" (+ 2.4 3) 5.4)
(test-is "+ 2b" (+ 2 3.4) 5.4)
(test-is "+ 2c" (+ 2.0 3.4) 5.4)
(test-is "+ 3" (+ 3 34 56) 93)
(test-is "+ 4" (+ 4 16 64 256) 340)
(test-is "* 0" (*) 1)
(test-is "* 1" (* 22.3) 22.3)
(test-is "* 2" (* 3 4) 12)
(test-is "* 2a" (* 3.0 4) 12)
(test-is "* 2b" (* 3 4.0) 12)
(test-is "* 2c" (* 3.0 4.0) 12)
(test-is "* 3" (* 1 2 3) 6)
(test-is "* 7" (* 1 2 3 4 5 6 7) 5040)

(test-is "factor 1e14 + 1" (factor 100000000000001) '(29 101 281 121499449))

(test-is "abs 0" (abs 0) 0)
(test-is "abs 1" (abs 1) 1)
(test-is "abs 1.5" (abs 1.5) 1.5)
(test-is "abs -1" (abs -1) 1)
(test-is "abs -1.5" (abs -1.5) 1.5)
(test-err "abs nil" (abs nil) #/is not a number/)
(test-err "abs \"huhu\"" (abs "huhu") #/is not a number/)

(test-is "signum 0" (signum 0) 0)
(test-is "signum 1" (signum 1) 1)
(test-is "signum -1" (signum -1) -1)
(test-is "signum 0.0" (signum 0.0) 0)
(test-is "signum 0E0" (signum 0E0) 0)
(test-is "signum 14.25" (signum 14.25) 1)
(test-is "signum -pi" (signum -3.141592653589793) -1)

(test-not "oddp 0" (oddp (* 3 0)))
(test "oddp 1" (oddp (* 1 1)))
(test-not "oddp 2" (oddp (* 2 1)))
(test "oddp 3" (oddp (* 3 1)))
(test-not "oddp 4" (oddp (* 2 2)))
(test "oddp -1" (oddp (* 1 -1)))
(test-not "oddp -2" (oddp (* 2 -1)))
(test "oddp -3" (oddp (* 3 -1)))
(test-not "oddp 0.0" (oddp 0.0))
(test-not "oddp 0.1" (oddp 0.1))
(test-not "oddp -0.3" (oddp -0.3))
(test-not "oddp -119.5" (oddp (* -1 119.5)))

(test "evenp 0" (evenp (* 3 0)))
(test-not "evenp 1" (evenp (* 1 1)))
(test "evenp 2" (evenp (* 2 1)))
(test-not "evenp 3" (evenp (* 3 1)))
(test "evenp 4" (evenp (* 2 2)))
(test-not "evenp -1" (evenp (* 1 -1)))
(test "evenp -2" (evenp (* 2 -1)))
(test-not "evenp -3" (evenp (* 3 -1)))
(test "evenp 0.0" (evenp 0.0))
(test-not "evenp 0.1" (evenp 0.1))
(test-not "evenp -0.3" (evenp -0.3))
(test-not "evenp -119.5" (evenp (* -1 119.5)))

(done-testing)
