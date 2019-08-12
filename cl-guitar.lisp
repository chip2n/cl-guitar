(in-package #:cl-guitar)

(defconstant +svg-doctype+
  "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">")
(defconstant +svg-header+ "<?xml version=\"1.0\" standalone=\"yes\"?>~%~%")

(defvar *diagrams* (make-hash-table :test #'equal))
(defparameter *diagram-dir* (asdf:system-relative-pathname ) #P".")

(defparameter *svg* *standard-output*)
(defparameter *width* 212)
(defparameter *height* 395)

(defmacro with-svg (&body body)
  `(progn
     (format *svg* +svg-header+)
     (who:with-html-output
	 (*svg* nil
                :prologue +svg-doctype+
                :indent T)
       ,@body)))

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
  (destructuring-bind (string fret label &optional (type :filled)) note
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

(defun svg->png (source destination)
  (uiop:run-program (format nil "inkscape -z -e ~a ~a" destination source)))

(defclass diagram ()
  ((id :initarg :id
       :reader diagram-id)
   (title :initarg :title)
   (frets :initarg :frets)
   (notes :initarg :notes)))

(defun create-diagram (diagram destination)
  (with-slots (title frets notes) diagram
    (uiop:call-with-temporary-file
     (lambda (stream path)
       (let ((*svg* stream))
         (save-diagram title frets notes))
       (finish-output stream)
       (svg->png path destination))
     :direction :output
     :type "svg")))

(defun push-diagram (diagram)
  (let ((place (gethash (diagram-id diagram) *diagrams*)))
    (if place
        (warn "Replacing diagram ~a" (diagram-id diagram)))
    (setf (gethash (diagram-id diagram) *diagrams*) diagram)))

(defun add-diagram (id title frets notes)
  (let ((diagram (make-instance 'diagram
                                :id id
                                :title title
                                :frets frets
                                :notes notes))
        (output (merge-pathnames *diagram-dir* (format nil "~a.png" id))))
    (push-diagram diagram)
    (format t "Saving ~a to ~a" id output)
    (create-diagram diagram output)))

(defmacro defdiagram (id (&key title frets) &body body)
  `(progn
     (add-diagram ,id ,title ,frets
                     (list ,@(loop for note in body
                                   collect `(make-note ',note))))))
