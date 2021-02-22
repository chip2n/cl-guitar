(in-package #:guitar)

(defparameter *bg-color* "#F9F5ED")
(defparameter *fg-color* "#615440")

;; * Colors

(defparameter *colors-alist*
  '((:highlight . "#FB4F4F")
    (:ref       . "#E1D0B1")
    (:new       . "#529DC7")
    (t          . "#615440")))

(defun color-lookup (keyword)
  (cdr (assoc keyword *colors-alist*)))

(defmacro diagram ((&key title frets (start 1) indicator) &body notes)
  `(let ((*svg* (or *svg* *standard-output*))
         (*width* 280)
         (*height* 400))
     (compile-svg 280 400 (guitar-diagram
                           :title ,title
                           :num-frets ,frets
                           :start-fret ,start
                           :fret-indicator ,indicator
                           :notes ',notes))))

(defun fretboard (num-frets &optional (draw-nut-p t))
  (let* ((nut-height (if draw-nut-p 16 0))
         (string-width 2)
         (fret-height 1.54052)
         (num-strings 6))
    (svg
      ;; Draw the nut
      (if draw-nut-p
          (rect :width *width*
                :height nut-height
                :fill *fg-color*)
          (rect :width *width*
                :height 2
                :fill *fg-color*))

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
  (if (note-muted-p note)
      :muted
      (or (cadddr note) :filled)))

(defun note-string (note)
  (car note))

(defun note-muted-p (note)
  (and (symbolp (cadr note))
       (string= (symbol-name (cadr note)) "X")))

(defun note-fret (note)
  (if (note-muted-p note)
      0
      (cadr note)))

(defun note (x y radius note)
  (let* ((style (note-style note))
         (real-style (if (listp style)
                         (car style)
                         style))
         (color (color-lookup (if (listp style) (cadr style) t))))
    (ecase real-style
      (:filled (svg
                 (circle :x x :y y :radius radius :fill color :stroke *bg-color*)
                 (text (note-label note) :x x :y y :fill *bg-color* :size 16)))
      (:stroked (svg
                  (circle :x x :y y :radius radius :fill *bg-color* :stroke color :stroke-width 2)
                  (text (note-label note) :x x :y y :fill color :size 16)))
      (:dashed (svg
                 (circle :x x :y y :radius radius :fill *bg-color* :stroke color :stroke-width 2 :dashedp t)
                 (text (note-label note) :x x :y y :fill color :size 16)))
      (:muted (svg
                (group (:x x :y y :rotation 45)
                       (line :x1 0 :y1 (- 0 radius) :x2 0 :y2 (+ 0 radius) :stroke color :stroke-width 2)
                       (line :x1 (- 0 radius) :y1 0 :x2 (+ 0 radius) :y2 0 :stroke color :stroke-width 2)))))))

(defun guitar-diagram (&key title (num-frets 5) (num-strings 6) (start-fret 0) fret-indicator notes)
  (let* ((note-radius 16)
         (padding (+ (* note-radius 2) 16))
         (nut-height (if (= start-fret 1) 16 0)))
    (flet ((calc-note-coords (note)
             (let* ((string-index (note-string note))
                    (fret-index (if (= (note-fret note) 0)
                                    0
                                    (+ (- (note-fret note) start-fret) 1)))
                    (fret-space (/ *height* (float num-frets)))
                    (x (* (- num-strings string-index)
                          (/ *width* (float (- num-strings 1)))))
                    (y (if (eq fret-index 0) ; handling open notes differently
                           (/ padding 2)
                           (- (* fret-index fret-space)
                              (/ fret-space 2)))))
               `(,x ,y))))
      (multiple-value-bind (open-notes fretted-notes)
          (serapeum:partition (lambda (x) (eq (note-fret x) 0)) notes)
        (svg
          ;; Draw background
          (rect :width *width* :height *height* :fill *bg-color*)

          ;; Draw title
          (when title
            (text title :x (/ *width* 2) :y (- *height* (/ padding 2)) :fill *fg-color*))

          ;; Draw fret indicator
          (when fret-indicator
            (inset (0 (* padding 2))
              (inset (0 nut-height :bottom)
                (when (> start-fret 0)
                  (text (format nil "~afr" fret-indicator)
                        :x (- *width* 16)
                        :y (+ (* (- fret-indicator start-fret) (/ *height* (float num-frets)))
                              (/ *height* (float num-frets) 2))
                        :fill *fg-color*)))))

          (inset ((* padding 2) 0)
            (loop for note in open-notes
                  for (x y) = (calc-note-coords note)
                  collect (note x y note-radius note))

            (inset (0 (* padding 2) :center)
              (fretboard num-frets (= start-fret 1))

              ;; TODO nut height constant
              (inset (0 nut-height :bottom)

                (loop for note in fretted-notes
                      for (x y) = (calc-note-coords note)
                      collect (note x y note-radius note))))))))))
