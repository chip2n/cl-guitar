(in-package #:cl-guitar)

(defvar *svg* nil)
(defvar *bg-color* "#F9F5ED")
(defvar *fg-color* "#7B6B52")

(defvar *width* 212)
(defvar *height* 395)

(defun emit-svg (tree)
  (when tree
    (if (stringp tree)
        (format *svg* "~a" tree)
        (let ((tag (string-downcase (car tree)))
              (args (mapcar (lambda (x)
                              (cons (string-downcase (car x)) (cdr x)))
                            (c:group (cadr tree) 2)))
              (body (cddr tree)))
          (format *svg* "<~a ~:{~a='~a' ~}>" tag args)
          (loop for form in body do
            (emit-svg form))
          (format *svg* "</~a>" tag)))))

(defun rect->svg (x y width height fill)
  `(:rect (:x ,x :y ,y :width ,width :height ,height :fill ,fill)))

(defun circle->svg (x y r fill stroke stroke-width)
  `(:circle (:cx ,x
             :cy ,y
             :r ,r
             :fill ,fill
             :stroke ,stroke
             :stroke-width ,stroke-width
             :stroke-dasharray "4,4")))

(defun line->svg (x1 y1 x2 y2 stroke stroke-width)
  `(:line (:x1 ,x1
           :y1 ,y1
           :x2 ,x2
           :y2 ,y2
           :stroke ,stroke
           :stroke-width ,stroke-width)))

(defun text->svg (s x y fill)
  `(:text (:x ,x
           :y ,y
           :font-family "Noto Sans"
           :font-size 14
           :font-weight "bold"
           :text-anchor "middle"
           :fill ,fill
           :dominant-baseline "central")
          ,s))

(defun group->svg (attrs tree)
  (destructuring-bind (&key x y rotation) attrs
    `(:g (:transform ,(format nil "translate(~a ~a) rotate(~a)" x y rotation))
         ,@(mapcar #'node->svg tree))))

(defun node->svg (node)
  (if (listp (car node))
      (group->svg '(:x 0 :y 0 :rotation 0) node)
      (ecase (car node)
        (:rect (apply #'rect->svg (cdr node)))
        (:circle (apply #'circle->svg (cdr node)))
        (:line (apply #'line->svg (cdr node)))
        (:text (apply #'text->svg (cdr node)))
        (:group (group->svg (cadr node) (caddr node))))))

(defun ast->svg (width height tree)
  (let ((data (mapcar #'node->svg tree)))
    `(:svg (:xmlns "http://www.w3.org/2000/svg"
            :|xmlns:xlink| "http://www.w3.org/1999/xlink"
            :width ,width
            :height ,height
            :version "1.1")
       ,@data)))

(defun compile-svg (ast)
  (->> ast
    (ast->svg *width* *height*)
    (emit-svg)))

(defmacro svg (&body body)
  (alexandria:with-gensyms (cmds)
    `(let ((,cmds nil))
       (macrolet ((rect (&key (x 0) (y 0) width height fill)
                    `(list :rect ,x ,y ,width ,height ,fill))
                  (circle (&key (x 0) (y 0) radius fill stroke (stroke-width 0))
                    `(list :circle ,x ,y ,radius ,fill ,stroke ,stroke-width))
                  (line (&key (x1 0) (y1 0) (x2 0) (y2 0) stroke (stroke-width 1))
                    `(list :line ,x1 ,y1 ,x2 ,y2 ,stroke ,stroke-width))
                  (text (s &key (x 0) (y 0) fill)
                    `(list :text ,s ,x ,y ,fill))
                  (group ((&key (x 0) (y 0) (rotation 0)) &body body)
                    `(list :group (list :x ,x :y ,y :rotation ,rotation) (list ,@body))))
         ,@(mapcar (lambda (form) `(push ,form ,cmds)) body)
         (nreverse ,cmds)))))

(defmacro inset ((dx dy &optional (anchor :center)) &body body)
  (alexandria:with-gensyms (gdx gdy)
    (let ((group-args
            (ecase anchor
              (:center `(:x (/ ,gdx 2) :y (/ ,gdy 2)))
              (:bottom `(:x (/ ,gdx 2) :y ,gdy)))))
      `(let* ((,gdx ,dx)
              (,gdy ,dy)
              (*width* (- *width* ,gdx))
              (*height* (- *height* ,gdy)))
         (group ,group-args ,@body)))))

;; guitar specific stuff -------

(progn
  (defmacro diagram ((&key title frets) &body notes)
    `(let ((*svg* (or *svg* *standard-output*))
           (*width* 280)
           (*height* 400))
       (compile-svg (guitar-diagram :title ,title :num-frets ,frets :notes ',notes))))

  (defun fretboard (num-frets)
    (let* ((nut-height 13.8647)
           (string-width 1.54052)
           (fret-height 1.54052)
           (num-strings 6))
      (svg
        ;; Draw the nut
        (rect :width *width*
              :height nut-height
              :fill *fg-color*)

        (inset (0 nut-height :bottom)

          ;; Draw the strings
          (loop for s from 0 below num-strings
                collect (rect :x (* s (/ (- *width* string-width) (float (- num-strings 1))))
                              :width string-width
                              :height *height*
                              :fill *fg-color*))

          ;; Draw the frets
          (loop for s from 0 below num-frets
                collect (rect :y (* (+ s 1) (/ *height* (float num-frets)))
                              :width *width*
                              :height fret-height
                              :fill *fg-color*))))))

  (defun note-label (note)
    (caddr note))

  (defun note-style (note)
    (or (cadddr note) :filled))

  (defun note (x y radius note)
    (ecase (note-style note)
      (:filled (svg
                 (circle :x x :y y :radius radius :fill *fg-color* :stroke *bg-color*)
                 (text (note-label note) :x x :y y :fill *bg-color*)))
      (:dashed (svg
                 (circle :x x :y y :radius radius :fill *bg-color* :stroke *fg-color* :stroke-width 2)
                 (text (note-label note) :x x :y y :fill *fg-color*)))
      (:muted (svg
                (group (:x x :y y :rotation 45)
                       (line :x1 0 :y1 (- 0 radius) :x2 0 :y2 (+ 0 radius) :stroke *fg-color* :stroke-width 2)
                       (line :x1 (- 0 radius) :y1 0 :x2 (+ 0 radius) :y2 0 :stroke *fg-color* :stroke-width 2))))))

  (defun guitar-diagram (&key title (num-frets 5) (num-strings 6) notes)
    (let* ((note-radius 16)
           (padding (+ (* note-radius 2) 8))
           (nut-height 13.8647))
      (flet ((calc-note-coords (note)
               (let* ((string-index (car note))
                      (fret-index (cadr note))
                      (fret-space (/ *height* (float num-frets)))
                      (x (* (- num-strings string-index)
                            (/ *width* (float (- num-strings 1)))))
                      (y (if (eq fret-index 0) ; handling open notes differently
                             (/ padding 2)
                             (- (* fret-index fret-space)
                                (/ fret-space 2)))))
                 `(,x ,y))))
        (multiple-value-bind (open-notes fretted-notes)
            (serapeum:partition (lambda (x) (eq (cadr x) 0)) notes)
          (svg
            ;; Draw background
            (rect :width *width* :height *height* :fill *bg-color*)

            ;; Draw title
            (when title
              (text title :x (/ *width* 2) :y (- *height* (/ padding 2)) :fill *fg-color*))

            (inset ((* padding 2) 0)
              (loop for note in open-notes
                    for (x y) = (calc-note-coords note)
                    collect (note x y note-radius note))

              (inset (0 (* padding 2) :center)
                (fretboard num-frets)

                ;; TODO nut height constant
                (inset (0 nut-height :bottom)

                  (loop for note in fretted-notes
                        for (x y) = (calc-note-coords note)
                        collect (note x y note-radius note))))))))))

  (c:dump-output (s "/tmp/diagram.html")
    (let ((*svg* s))
      (diagram (:title "A minor (open)" :frets 5)
        (1 0 "R" :muted)
        (5 0 "R" :dashed)
        (4 2 "5")
        (3 2 "R")
        (2 1 "b3")))))
