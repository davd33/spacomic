(ql:quickload "cl-ansi-term")
(ql:quickload "spacomic")

(term:cat-print "===START=PROGRAM===")

(setf *random-state* (make-random-state t))

;; (setf swank::*loopback-interface* "0.0.0.0")
(swank:create-server :port 4005 :dont-close t)
(read-char)
(spacomic:MAIN)
