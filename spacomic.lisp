;;;; spacomic.lisp

(in-package #:spacomic)

(defvar *game-win* nil)

(defvar *debug-mode* nil)

;; LETTERS
(defmethod make-letter ((segments list))
  "Takes a list of segments and return a function that,
   when given a rectangle, draws itself on the window.
   A rectangle is a 3 elements list: a start-point, a width and a height."
  (lambda (rect)
    ;; a rectangle has a start-point, a width and a height
    (destructuring-bind ((ry0 rx0) rw rh) rect
      (draw-letter (loop for seg in segments
                         collect (destructuring-bind ((ya xa) (yb xb)) seg
                                   ;; Building a segment: a list of two points.
                                   (let ((YA (round (+ ry0 (* rh ya))))
                                         (XA (round (+ rx0 (* rw xa))))
                                         (YB (round (+ ry0 (* rh yb))))
                                         (XB (round (+ rx0 (* rw xb)))))
                                     `((,YA ,XA)
                                       (,YB ,XB)))))))))

(defparameter letters
  `(:a ,(make-letter '(((1 0) (0 1/2))
                       ((1/2 1/4) (1/2 3/4))
                       ((0 1/2) (1 1))))
    :b ,(make-letter '(((1 1/5) (0 1/5))
                       ((0 1/5) (0 4/5))
                       ((0 4/5) (1/5 1))
                       ((1/5 1) (2/5 1))
                       ((2/5 1) (5/10 4/5))
                       ((5/10 4/5) (3/5 1))
                       ((3/5 1) (4/5 1))
                       ((4/5 1) (1 4/5))
                       ((1 4/5) (1 1/5))))
    :c '((0 0) (0 0))
    :d '((0 0) (0 0))
    :e '((0 0) (0 0))
    :f '((0 0) (0 0))
    :g '((0 0) (0 0))
    :h '((0 0) (0 0))
    :i '((0 0) (0 0))
    :j '((0 0) (0 0))
    :k '((0 0) (0 0))
    :l '((0 0) (0 0))
    :m '((0 0) (0 0))
    :n '((0 0) (0 0))
    :o '((0 0) (0 0))
    :p '((0 0) (0 0))
    :q '((0 0) (0 0))
    :r '((0 0) (0 0))
    :s '((0 0) (0 0))
    :t '((0 0) (0 0))
    :u '((0 0) (0 0))
    :v '((0 0) (0 0))
    :w '((0 0) (0 0))
    :x '((0 0) (0 0))
    :y '((0 0) (0 0))
    :z '((0 0) (0 0))
    :0 '((0 0) (0 0))
    :1 '((0 0) (0 0))
    :2 '((0 0) (0 0))
    :3 '((0 0) (0 0))
    :4 '((0 0) (0 0))
    :5 '((0 0) (0 0))
    :6 '((0 0) (0 0))
    :7 '((0 0) (0 0))
    :8 '((0 0) (0 0))
    :9 '((0 0) (0 0))))

;; DRAW THINGS
(defun draw-segment (seg)
  (destructuring-bind ((ya xa) (yb xb)) seg
    (croatoan:draw-shape *game-win*
                         (croatoan:line ya xa yb xb))))

(defun draw-letter (letter)
  (loop for segment in letter
        do (draw-segment segment)))

;; UTILITIES
(defun make-keyword (name)
  (read-from-string (reverse (concatenate 'string
                                          (reverse name) ":"))))

;; RECTANGLE

(defun make-rect (y x w h)
  (list (list y x) w h))

(defun yrect (r)
  (caar r))
(defun xrect (r)
  (cadar r))
(defun origrect (r)
  (car r))
(defun wrect (r)
  (cadr r))
(defun hrect (r)
  (caddr r))

;; DISPLAY TITLE
(defun str->letters (string)
  (loop for c across string
        collect (make-keyword (string c))))

(defun draw-title (string rect)
  (loop for letter in (str->letters string)
        for i from 1 to (length string)
        do (funcall (getf letters letter)
                    (let ((rect-length (/ (xrect rect) (length string))))
                      (make-rect (yrect rect)
                                 (* rect-length i)
                                 rect-length
                                 (wrect rect))))))

;; MAIN
(defun run ()
  (croatoan:with-screen (screen
                         :input-echoing nil
                         :enable-function-keys t)
    (croatoan:with-window (win)
      (croatoan:box win)
      (let ((*game-win* win))
        (loop while t
              do (let ((c (croatoan:get-char win)))
                   (draw-title "ab" '((0 0) 30 20))))))))

;; ENTRY
(defun -main ()
  (when (not *debug-mode*) (sb-ext:disable-debugger))
  (setf *random-state* (make-random-state t))
  (setf swank::*loopback-interface* "0.0.0.0")
  (swank:create-server :port 4005 :dont-close t)
  (run)
  (sb-ext:exit))

