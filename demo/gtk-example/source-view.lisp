(defpackage :source-view-example
  (:use :gtk :gdk :gobject :glib :pango :cairo :cffi :iterate :common-lisp)
  (:export #:source-view-example))


(defun source-view-example ()
  (within-main-loop
    (let* ((window (make-instance 'gtk-window
				  :type :toplevel
				  :title "Hello, SourceView"
				  :default-width  500
				  :default-height 300))
	   (box (make-instance 'gtk-box
			       :orientation :horizontal))
	   (source (gtk-source-view-new)))
      ;; (setf (gtk-source-view-show-line-numbers source) t)
      (setf (gtk-source-view-tab-width source) 4)
      (setf (gtk-source-view-highlight-current-line source) nil)
      
      ;; (gtk-source-view-set-background-pattern source t)
      ;; (gtk-source-view-set-highlight-current-line source t)
      (gtk-box-pack-start box source)
      ;; (setf (gtk-source-view-editable source) nil)
      ;; (bt:make-thread (lambda ()
			;; (dotimes (x 10)
			  ;; (sleep 1)
			  ;; (gdk-threads-add-idle
			   ;; (lambda ()
			     ;; (format t "---~%~a~%===" (gtk:gtk-source-buffer-text (gtk-source-view-buffer source))))))))
      ;; (gtk-source-buffer-set-language (gtk-source-view-buffer source) 1)
      
      (g-signal-connect window "destroy"
			(lambda (widget)
			  (declare (ignore widget))
			  (leave-gtk-main)))
      (gtk-container-add window box)
      (gtk-widget-show-all window))
    ))
