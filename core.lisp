(in-package #:cl-guitar)

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


;; ----


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

(defmacro diagram ((&key title (count 4) (start 0) indicator) &body body)
  `(save-diagram
    ,title
    ,count
    (list ,@(loop :for note :in body :collect `(make-note ',note)))))

(defmacro svg ((&key width height) &body body)
  `(let ((tree (list ,@body)))
     (emit-svg
      `(:svg (:xmlns "http://www.w3.org/2000/svg"
             :|xmlns:xlink| "http://www.w3.org/1999/xlink"
             :width ,,width
             :height ,,height
             :version "1.1")
        ,@tree))))

(defun rect (&key width height fill)
  `(:rect (:width ,width
           :height ,height
           :fill ,fill)))

(defun text (s &key x y color)
  `(:text (:x ,x
           :y ,y
           :font-family "Noto Sans"
           :font-size 14
           :font-weight "bold"
           :text-anchor "middle"
           :fill ,color
           :dominant-baseline "central")
          ,s))

(defmacro offset ((&key (x 0) (y 0)) &body body)
  (alexandria:with-gensyms (gx gy)
    `(let ((,gx ,x)
           (,gy ,y))
      `(:g (:transform ,(format nil "translate(~a ~a)" ,gx ,gy))
           ,,@body))))

#+nil
(c:dump-output (s "/tmp/diagram.html")
  (let ((*svg* s))
    (svg (:width 200 :height 200)
      (rect :width 100 :height 100)
      (text "hello" :x 10 :y 10 :color "#0000ff")

      (offset (:x 40 :y 40)
        (text "hello2" :x 0 :y 0 :color "#0000ff"))

      ;; TODO handle list of lists
      (loop for x from 0 below 6
            collect (rect :width 2 :height 100 :fill "#ff0000"))

      ;; `(:g (:transform ,(format nil "translate(~a ~a)" 40 40))
      ;;     ,(text "hello2" :x 0 :y 0 :color "#0000ff"))
      )))




;; (with-open-file (s "/tmp/diagram.html" :direction :output
;;                                       :if-does-not-exist :create
;;                                       :if-exists :supersede)
;;   (let ((*svg* s))
;;     (diagram (:title "Amin chord"
;;               :Start 0
;;               :count 4
;;               :indicator 5)
;;       (6 0 nil :muted)
;;       (5 0 "R")
;;       (4 2 "5")
;;       (3 2 "R")
;;       (2 1 "b3")
;;       (1 0 "5"))))

;; (with-open-file (s "/tmp/diagram.html" :direction :output
;;                                        :if-does-not-exist :create
;;                                        :if-exists :supersede)
;;   (let ((*svg* s)
;;         (img-width 200)
;;         (img-height 200)
;;         (bg-color "#ff0000")
;;         (fg-color "#00ff00")
;;         (title "hello"))
;;     (svg (:width img-width :height img-height)

;;       (rect :width img-width
;;             :height img-height
;;             :fill "#003300")

;;       ;; (who:htm
;;       ;;   (:rect :width img-width
;;       ;;         :height img-height
;;       ;;          :fill bg-color))

;;          (:text :x (/ img-width 2.0)
;;                 :y 20
;;                 :font-family "Noto Sans"
;;                 :font-size 14
;;                 :font-weight "bold"
;;                 :text-anchor "middle"
;;                 :fill fg-color
;;                 :dominant-baseline "central"
;;                 (who:str title))
;;          ;; (:g :transform (format nil "translate(~a ~a)" padding padding)

;;          ;;     (:rect :width width
;;          ;;            :height nut-height
;;          ;;            :fill fg-color)








;;          ;;     )
;;       )))




;; (defmacro svg ((&key width height) &body body)
;;   `(who:with-html-output (*svg* nil :indent T)
;;      (:svg
;;       :xmlns "http://www.w3.org/2000/svg"
;;       :|xmlns:xlink| "http://www.w3.org/1999/xlink"
;;       :width ,width
;;       :height ,height
;;       :version "1.1"
;;       ,@body)))

;; (defun svg->png (source destination) ||#
;;   (uiop:run-program (format nil "inkscape -z -e ~a ~a" destination source))) ||#

;; (defun create-diagram (title num-frets destination notes) ||#
;;   (uiop:call-with-temporary-file ||#
;;    (lambda (stream path) ||#
;;      (let ((*svg* stream)) ||#
;;        (save-diagram title num-frets notes)) ||#
;;      (finish-output stream) ||#
;;      (svg->png path destination)) ||#
;;    :direction :output ||#
;;    :type "svg")) ||#


;; (defdiagram (:title "minMaj7"  ||#
;;              :frets 6) ||#
;;   (1 6 "R" :dashed) ||#
;;   (2 1 "R" :dashed) ||#
;;   (4 4 "R" :dashed) ||#
;;   (2 4 "b3") ||#
;;   (3 3 "5") ||#
;;   (4 3 "7")) ||#

;; (defdiagram (:title "min6/9" 
;;                     :frets 6)
;;   (1 4 "R" :dashed)
;;   (4 2 "R" :dashed)
;;   (3 6 "R" :dashed)
;;   (2 2 "b3")
;;   (3 3 "6")
;;   (4 4 "2"))
