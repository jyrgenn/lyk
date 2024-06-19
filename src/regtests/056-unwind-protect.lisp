(require 'regtests)

(defparameter uw-cleanup nil)
(test-is "unwind-protect 1" (unwind-protect
                                (* 113 115)
                              (setq uw-cleanup 1))
         12995)
(test-is "unwind-protect 1a" uw-cleanup 1)
(test-is "unwind-protect 2" (unwind-protect
                                (* 113 113)
                              (setq uw-cleanup 1)
                              (incf uw-cleanup 15))
         12769)
(test-is "unwind-protect 2a" uw-cleanup 16)

(test-is "unwind-protect 3" (errset (unwind-protect
                                        (progn
                                          (error "throw *this*!")
                                          (* 113 115))
                                      (setq uw-cleanup 2)) nil)
         nil)
(test-is "unwind-protect 3a" uw-cleanup 2)
(test-is "unwind-protect 4" (errset (unwind-protect
                                        (progn
                                          (* 113 113)
                                          (error "throw *this*!"))
                                      (setq uw-cleanup 2)
                                      (incf uw-cleanup 15)) nil)
         nil)
(test-is "unwind-protect 4a" uw-cleanup 17)

(defparameter uw-cleanup2 nil)
(test-is "unwind-protect 5" (errset (unwind-protect
                                        (progn
                                          (* 113 113)
                                          (unwind-protect
                                              (error "throw *this*!")
                                            (setq uw-cleanup2 'bla)))
                                      (setq uw-cleanup 2)
                                      (incf uw-cleanup 15)) nil)
         nil)
(test-is "unwind-protect 5a" (cons uw-cleanup uw-cleanup2) '(17 . bla))

(done-testing)
