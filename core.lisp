(in-package #:cl-guitar)

(defvar *svg* *standard-output*)
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
  (destructuring-bind (&key x y) attrs
    `(:g (:transform ,(format nil "translate(~a ~a)" x y))
         ,@(mapcar #'node->svg tree))))

(defun node->svg (node)
  (if (listp (car node))
      (group->svg '(:x 0 :y 0) node)
      (ecase (car node)
        (:rect (apply #'rect->svg (cdr node)))
        (:circle (apply #'circle->svg (cdr node)))
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

;; --------------

(defmacro svg (&body body)
  (alexandria:with-gensyms (cmds)
    `(let ((,cmds nil))
       (macrolet ((rect (&key (x 0) (y 0) width height fill)
                    `(list :rect ,x ,y ,width ,height ,fill))
                  (circle (&key (x 0) (y 0) radius fill stroke (stroke-width 0))
                    `(list :circle ,x ,y ,radius ,fill ,stroke ,stroke-width))
                  (text (s &key (x 0) (y 0) fill)
                    `(list :text ,s ,x ,y ,fill))
                  (group ((&key (x 0) (y 0)) &body body)
                    `(list :group (list :x ,x :y ,y) (list ,@body))))
         ,@(mapcar (lambda (form) `(push ,form ,cmds)) body)
         (nreverse ,cmds)))))

(defmacro inset ((dx dy &optional (anchor :center)) &body body)
  (alexandria:with-gensyms (gdx gdy)
    ;; (let ((group-attr (ecase anchor
    ;;                     (:center (:x (/ ,gdx 2) :y (/ ,gdy 2)))
    ;;                     (:top (:x (/ ,gdx 2) :y 0))))))
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

  (defun guitar-diagram (&key title (num-frets 5) (num-strings 6) notes)
    (let* ((note-radius 16)
           (padding (+ (* note-radius 2) 8)))
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
                  collect (let* ((string-index (car note))
                                 (fret-index (cadr note))
                                 (fret-space (/ *height* (float num-frets)))
                                 (note-x (* (- num-strings string-index)
                                            (/ *width* (float (- num-strings 1)))))
                                 (note-y (/ padding 2))
                                 (type :circle)
                                 (note-label (caddr note)))
                            (group (:x 0 :y 0)
                                   (circle :x note-x :y note-y :radius note-radius :fill "#ff0000")
                                   (text note-label :x note-x :y note-y :fill "#ffffff"))))

            (inset (0 (* padding 2) :center)
              (fretboard num-frets)

              ;; TODO nut height constant
              (inset (0 13.8647 :bottom)

                (loop for note in fretted-notes
                      collect (let* ((string-index (car note))
                                     (fret-index (cadr note))
                                     (fret-space (/ *height* (float num-frets)))
                                     (note-x (* (- num-strings string-index)
                                                (/ *width* (float (- num-strings 1)))))
                                     (note-y (- (* fret-index fret-space)
                                                (/ fret-space 2)))
                                     (type :circle)
                                     (note-label (caddr note)))
                                (group (:x 0 :y 0)
                                       (circle :x note-x :y note-y :radius note-radius :fill "#ff0000")
                                       (text note-label :x note-x :y note-y :fill "#ffffff")))))))))))

  (c:dump-output (s "/tmp/diagram.html")
    (let ((*svg* s)
          (*width* 280)
          (*height* 400)
          (notes '((5 0 "R")
                   (4 2 "5")
                   (3 2 "R")
                   (2 1 "b3")))
          ;; (notes '((5 0 "R")
          ;;          (5 1 "R")
          ;;          (5 2 "R")))
          )
      (compile-svg (guitar-diagram :title "A minor (open)" :num-frets 5 :notes notes)))))
