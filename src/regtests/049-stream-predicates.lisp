;; predicates about streams

;; we can use *standard-outout*, *standard-input*, *error-output*,
;; *terminal-io* to test these predicates -- *and* the properties of
;; these streams

(defvar open-file-stream (open "/etc/passwd" :direction :input))
(defvar closed-file-stream (open "/etc/passwd" :direction :input))
(close closed-file-stream)


(test-is "input-stream-p 0" (input-stream-p *standard-input*) t)
(test-is "input-stream-p 1" (input-stream-p *standard-output*) nil)
(test-is "input-stream-p 2" (input-stream-p *error-output*) nil)
;; TODO (test-is "input-stream-p 3" (input-stream-p *terminal-io*) t)
(test-is "input-stream-p 4" (input-stream-p open-file-stream) t)
(test-is "input-stream-p 5" (input-stream-p closed-file-stream) t)

(test-is "output-stream-p 0" (output-stream-p *standard-input*) nil)
(test-is "output-stream-p 1" (output-stream-p *standard-output*) t)
(test-is "output-stream-p 2" (output-stream-p *error-output*) t)
;; well, maybe it is? Check the ConsoleReader more thoroughly some
;; time
;; TODO (test-is "output-stream-p 3" (output-stream-p *terminal-io*) nil)
(test-is "output-stream-p 4" (output-stream-p open-file-stream) nil)
(test-is "output-stream-p 5" (output-stream-p closed-file-stream) nil)

(test-is "interactive-stream-p 0" (interactive-stream-p *standard-input*) nil)
(test-is "interactive-stream-p 1" (interactive-stream-p *standard-output*) nil)
(test-is "interactive-stream-p 2" (interactive-stream-p *error-output*) nil)
;; TODO (test-is "interactive-stream-p 3" (interactive-stream-p *terminal-io*) t)
(test-is "interactive-stream-p 4" (interactive-stream-p open-file-stream) nil)
(test-is "interactive-stream-p 5" (interactive-stream-p closed-file-stream) nil)

(test-is "open-stream-p 0" (open-stream-p *standard-input*) t)
(test-is "open-stream-p 1" (open-stream-p *standard-output*) t)
(test-is "open-stream-p 2" (open-stream-p *error-output*) t)
;; TODO (test-is "open-stream-p 3" (open-stream-p *terminal-io*) t)
(test-is "open-stream-p 4" (open-stream-p open-file-stream) t)
(test-is "open-stream-p 5" (open-stream-p closed-file-stream) nil)

(done-testing)
