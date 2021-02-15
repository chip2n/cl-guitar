(in-package #:cl-guitar)

;; TODO remove cl-who

(alexandria:define-constant +svg-doctype+
  "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">"
  :test #'string=)

(alexandria:define-constant +svg-header+
  "<?xml version=\"1.0\" standalone=\"yes\"?>~%~%"
  :test #'string=)

(defparameter *svg* *standard-output*)
(defparameter *width* 212)
(defparameter *height* 395)

(defmacro with-svg (&body body)
  `(who:with-html-output (*svg* nil :indent T) ,@body))

(defun gen-line (fg-color)
  (let ((note-x 166.376)
        (note-y 170.998))
    `(:g
      (:circle :cx ,note-x
               :cy ,note-y
               :r 14.6349
               :fill ,fg-color)

      (:text :x ,note-x
             :y ,note-y
             :font-family "Noto Sans"
             :font-size 16
             :text-anchor "middle"
             :dominant-baseline "central"
             "b3"))))


(defclass fretted-note ()
  ((string-index
    :initarg :string-index
    :accessor string-index)
   (fret-index
    :initarg :fret-index
    :accessor fret-index)
   (note
    :initarg :note
    :accessor note)))

(defclass note ()
  ((label
    :initarg :label
    :accessor note-label)
   (type
    :initarg :type
    :accessor note-type)))

(defun make-note (note)
  (destructuring-bind (string fret &optional label (type :filled)) note
    (make-instance 'fretted-note
                   :string-index string
                   :fret-index fret
                   :note (make-instance 'note :label label :type type))))

(defun save-diagram (title num-frets notes)
  (let* ((width *width*)
         (height *height*)
         (padding 32)
         (img-height (+ height padding padding))
         (img-width (+ width padding padding))
         (fg-color "#7B6B52")
         (bg-color "#F9F5ED")
         (nut-height 13.8647)
         (string-width 1.54052)
         (fret-height 1.54052)
         (num-strings 6)
         (fretboard-height (- height
                              nut-height
                              fret-height))
         (fret-space (/ fretboard-height (float num-frets))))

    (with-svg
      (:svg :width img-width
            :height img-height
            :version "1.1"
            :xmlns "http://www.w3.org/2000/svg" :|xmlns:xlink| "http://www.w3.org/1999/xlink"

            (:rect :width img-width
                   :height img-height
                   :fill bg-color)

            (:text :x (/ img-width 2.0)
                   :y 20
                   :font-family "Noto Sans"
                   :font-size 14
                   :font-weight "bold"
                   :text-anchor "middle"
                   :fill fg-color
                   :dominant-baseline "central"
                   (who:str title))
            (:g :transform (format nil "translate(~a ~a)" padding padding)

                (:rect :width width
                       :height nut-height
                       :fill fg-color)



                (loop for s from 0 below num-strings
                   do (who:htm
                       (:rect :width string-width
                              :x (* s (/ (- width string-width) (float (- num-strings 1))))
                              :height height
                              :fill fg-color)))

                (loop for s from 0 below num-frets
                   do
                     (who:htm
                      (:rect :width width
                             :height fret-height
                             :y (+ nut-height (* (+ s 1) (/ fretboard-height (float num-frets))))
                             :fill fg-color)))


                (loop for note in notes
                   do (let* ((note-x (* (string-index note)
                                        (/ width (float (- num-strings 1)))))
                             (note-y (- (+ nut-height (* (fret-index note)
                                                         (/ fretboard-height (float num-frets)))
                                           fret-height)
                                        (/ fret-space 2)))
                             (type (note-type (note note)))
                             (note-label (note-label (note note))))

                        (ecase type
                          (:muted (who:htm
                                   (:circle :cx note-x
                                            :cy note-y
                                            :r 14.6349
                                            :fill bg-color
                                            :stroke fg-color
                                            :stroke-width 2
                                            :stroke-dasharray "4,4")
                                   (:text :x note-x
                                          :y note-y
                                          :font-family "Noto Sans"
                                          :font-size 14
                                          :text-anchor "middle"
                                          :fill fg-color
                                          :dominant-baseline "central"
                                          (who:str note-label))))
                          (:dashed (who:htm
                                    (:circle :cx note-x
                                             :cy note-y
                                             :r 14.6349
                                             :fill bg-color
                                             :stroke fg-color
                                             :stroke-width 2
                                             :stroke-dasharray "4,4")
                                    (:text :x note-x
                                           :y note-y
                                           :font-family "Noto Sans"
                                           :font-size 14
                                           :text-anchor "middle"
                                           :fill fg-color
                                           :dominant-baseline "central"
                                           (who:str note-label))))
                          (:filled (who:htm
                                    (:circle :cx note-x
                                             :cy note-y
                                             :r 14.6349
                                             :fill fg-color)
                                    (:text :x note-x
                                           :y note-y
                                           :font-family "Noto Sans"
                                           :font-size 14
                                           :font-weight "bold"
                                           :text-anchor "middle"
                                           :fill bg-color
                                           :dominant-baseline "central"
                                           (who:str note-label))))))))))))

(defmacro defdiagram ((&key title frets) &body body)
  `(save-diagram ,title ,frets
                 (list ,@(loop for note in body
                               collect `(make-note ',note)))))

(defmacro diagram ((&key title (count 4) (start 0) indicator) &body body)
  `(save-diagram
    ,title
    ,count
    (list ,@(loop :for note :in body :collect `(make-note ',note)))))



;; ---------------------------------------------------------

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

(defmacro compile-ast (&body body)
  (alexandria:with-gensyms (cmds)
    `(let ((,cmds nil))
       (macrolet ((rect (&key (x 0) (y 0) width height fill)
                    `(push `(:rect ,,x ,,y ,,width ,,height ,,fill) ,',cmds))
                  (text (s &key (x 0) (y 0) fill)
                    `(push `(:text ,,s ,,x ,,y ,,fill) ,',cmds))
                  (group ((&key (x 0) (y 0)) &body body)
                    `(push `(:group (:x ,,x :y ,,y) ,(compile-ast ,@body)) ,',cmds)))
         ,@body
         (nreverse ,cmds)))))

(defmacro svg ((&key width height) &body body)
  `(emit-svg (ast->svg ,width ,height (compile-ast ,@body))))

(defun rect->svg (x y width height fill)
  `(:rect (:x ,x :y ,y :width ,width :height ,height :fill ,fill)))

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
  (ecase (car node)
    (:rect (apply #'rect->svg (cdr node)))
    (:text (apply #'text->svg (cdr node)))
    (:group (group->svg (cadr node) (caddr node)))))

(defun ast->svg (width height tree)
  (let ((data (mapcar #'node->svg tree)))
    `(:svg (:xmlns "http://www.w3.org/2000/svg"
            :|xmlns:xlink| "http://www.w3.org/1999/xlink"
            :width ,width
            :height ,height
            :version "1.1")
       ,@data)))

(progn
  (defmacro draw-fretboard-stuff (width height num-frets)
    (let* ((padding 32)
           (img-height (+ height padding padding))
           (img-width (+ width padding padding))
           (fg-color "#7B6B52")
           (bg-color "#F9F5ED")
           (nut-height 13.8647)
           (string-width 1.54052)
           (fret-height 1.54052)
           (num-strings 6)
           (fretboard-height (- height nut-height fret-height))
           (fret-space (/ fretboard-height (float num-frets))))
      `(group ()
              (rect :width ,width :height ,height :fill ,bg-color)

              ;; Draw the nut
              (rect :width ,width
                    :height ,nut-height
                    :fill ,fg-color)

              ;; Draw the strings
              (loop for s from 0 below ,num-strings
                    do (rect :x (* s (/ (- ,width ,string-width) (float (- ,num-strings 1))))
                             :width ,string-width
                             :height ,height
                             :fill ,fg-color))

              ;; Draw the frets
              (loop for s from 0 below ,num-frets
                    do (rect :y (+ ,nut-height (* (+ s 1) (/ ,fretboard-height (float ,num-frets))))
                             :width ,width
                             :height ,fret-height
                             :fill ,fg-color)))))

  (defun draw-diagram (&key width height num-frets)
    (let* ((width *width*)
           (height *height*)
           (padding 32)
           (img-height (+ height padding padding))
           (img-width (+ width padding padding))
           (fg-color "#7B6B52")
           (bg-color "#F9F5ED")
           (nut-height 13.8647)
           (string-width 1.54052)
           (fret-height 1.54052)
           (num-strings 6)
           (fretboard-height (- height nut-height fret-height))
           (fret-space (/ fretboard-height (float num-frets))))
      (svg (:width width :height height)
        ;; (draw-fretboard-stuff width height num-frets)
        (group ()
               (rect :width width :height height :fill bg-color)

               ;; Draw the nut
               (rect :width width
                     :height nut-height
                     :fill fg-color)

               ;; Draw the strings
               (loop for s from 0 below num-strings
                     do (rect :x (* s (/ (- width string-width) (float (- num-strings 1))))
                              :width string-width
                              :height height
                              :fill fg-color))

               ;; Draw the frets
               (loop for s from 0 below num-frets
                     do (rect :y (+ nut-height (* (+ s 1) (/ fretboard-height (float num-frets))))
                              :width width
                              :height fret-height
                              :fill fg-color)))
        (draw-text-stuff))))

  (defmacro draw-text-stuff ()
    `(group ()
            (rect :width 50 :height 50 :fill "#00ff00")))

  (c:dump-output (s "/tmp/diagram.html")
    (let ((*svg* s))
      (draw-diagram :width *width* :height *height* :num-frets 5))))
