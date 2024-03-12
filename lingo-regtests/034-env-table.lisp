(require 'regtests)

(defvar lola 'dingeldangel)
(test-is "env-table root" (table-get (env-table) 'lola)
         'dingeldangel)

(test-is "env-table in let 1" (let ((a 'foomly))
                                (table-get (env-table) 'a))
         'foomly)

(defvar env1)
(test-is "env-table in let 2" (let ((a 'foomly))
                                (setf env1 (the-environment))
                                (table-get (env-table) 'lola))
         'dingeldangel)

(test-not "from current env" ((env-table) 'a))
(test-is "from saved env" ((env-table env1) 'a) 'foomly)

(done-testing)
