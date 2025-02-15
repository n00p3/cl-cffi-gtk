;;;; Theming/CSS Basics - 2021-11-5
;;;;
;;;; Gtk themes are written using CSS. Every widget is build of multiple items
;;;; that you can style very similarly to a regular website.

(in-package :gtk-example)

(defun example-css-basics (&optional application)
  (within-main-loop
    (let* ((window (make-instance 'gtk-window
                                  :type :toplevel
                                  :application application
                                  :title "Example CSS Basics"
                                  :default-height 420
                                  :default-width 600))
           (container (make-instance 'gtk-scrolled-window))
           (text (make-instance 'gtk-text-buffer))
           (child (make-instance 'gtk-text-view
                                 :buffer text))
           (provider (make-instance 'gtk-css-provider)))
      ;; Signal handler for the window to handle the "destroy" signal
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Signal handler changed for the buffer
      (g-signal-connect text "changed"
                        (lambda (buffer)
                          (format t "Signal changed for text buffer~%")
                          (let ((start (gtk-text-buffer-start-iter buffer))
                                (end (gtk-text-buffer-end-iter buffer)))
                            (gtk-text-buffer-remove-all-tags buffer start end)
                            (gtk-css-provider-load-from-data
                                provider
                                (gtk-text-buffer-get-text buffer start end nil))
                            (apply-css-to-widget provider window))))
      (gtk-text-tag-table-add (gtk-text-buffer-tag-table text)
                              (make-instance 'gtk-text-tag
                                             :name "warning"
                                             :underline :single))
      (gtk-text-tag-table-add (gtk-text-buffer-tag-table text)
                              (make-instance 'gtk-text-tag
                                             :name "error"
                                             :underline :error))
      (setf (gtk-text-buffer-text text)
            (read-file (sys-path "css-basics.css")))
      (g-signal-connect provider "parsing-error"
          (lambda (provider section err)
            (declare (ignore provider err))
            (let ((start (gtk-text-buffer-iter-at-line-index
                             text
                             (gtk-css-section-start-line section)
                             (gtk-css-section-start-position section)))
                  (end (gtk-text-buffer-iter-at-line-index
                           text
                           (gtk-css-section-end-line section)
                           (gtk-css-section-end-position section))))
              (format t "in signal parsing-error: ~A, ~A~%" start end)
              (gtk-text-buffer-apply-tag text "warning" start end)
              +gdk-event-stop+)))
      ;; Add the widgets to the window
      (gtk-container-add container child)
      (gtk-container-add window container)
      ;; Apply the provider to the window
      (apply-css-to-widget provider window)
      ;; Show the window
      (gtk-widget-show-all window))))
