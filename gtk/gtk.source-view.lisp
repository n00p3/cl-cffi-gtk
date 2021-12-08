;;; ----------------------------------------------------------------------------
;;; gtk.source-view.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2021 Dieter Kaiser
;;; Copyright (C) 2021 - 2021 Mateusz Głowski
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License for Lisp
;;; as published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version and with a preamble to
;;; the GNU Lesser General Public License that clarifies the terms for use
;;; with Lisp programs and is referred as the LLGPL.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
;;; ----------------------------------------------------------------------------
(in-package :gtk)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (handler-case
      (progn
	(gir:invoke ((gir:require-namespace "GtkSource") 'init))
	(register-object-type "GtkSourceView" 'gtk-source-view))
    (simple-error (condition)
      (format t "~a~%" condition)
      (format t "You will not be able to use GtkSourceView widget
Make sure you have required dependencies installed"))))

(define-g-object-class "GtkSourceView" gtk-source-view
    (:superclass gtk-text-view
     :export t
     :interfaces ()
     :type-initializer "gtk_source_view_get_type")
    ((show-line-numbers
      gtk-source-view-show-line-numbers
      "show-line-numbers" "gboolean" t t)
     (tab-width
      gtk-source-view-tab-width
      "tab-width" "guint" t t)
     (indent-width
      gtk-source-view-indent-width
      "indent-width" "gint" t t)
     (auto-indent
      gtk-source-view-auto-indent
      "auto-indent" "gboolean" t t)
     (insert-spaces-instead-of-tabs
      gtk-source-view-insert-spaces-instead-of-tabs
      "insert-spaces-instead-of-tabs" "gboolean" t t)
     (indent-on-tab
      gtk-source-view-indent-on-tab
      "indent-on-tab" "gboolean" t t)
     (highlight-current-line
      gtk-source-view-highlight-current-line
      "highlight-current-line" "gboolean" t t)
     (show-right-margin
      gtk-source-view-show-right-margin
      "show-right-margin" "gboolean" t t)
     (right-margin-position
      gtk-source-view-right-margin-position
      "right-margin-position" "gboolean" t t)
     (show-line-marks
      gtk-source-view-show-line-marks
      "show-line-marks" "gboolean" t t)
     (mark-attributes
      gtk-source-view-mark-attributes
      "mark-attributes" "gboolean" t t)
     (smart-backspace
      gtk-source-view-smart-backspace
      "smart-backspace" "gboolean" t t)
     (smart-home-end
      gtk-source-view-smart-home-end
      "smart-home-end" "gboolean" t t)
     (enable-snippets
      gtk-source-view-enable-snippets
      "enable-snippets" "gboolean" t t)
     (visual-column
      gtk-source-view-visual-column
      "visual-column" "gboolean" t t)
     (completion
      gtk-source-view-completion
      "completion" "gboolean" t t)
     (hover
      gtk-source-view-hover
      "hover" "gboolean" t t)
     (indenter
      gtk-source-view-indenter
      "indenter" "gboolean" t t)
     (gutter
      gtk-source-view-gutter
      "gutter" "gboolean" t t)
     (background-pattern
      gtk-source-view-background-pattern
      "background-pattern" "gboolean" t t)
     (space-drawer
      gtk-source-view-space-drawer
      "space-drawer" "gboolean" t t)))


(declaim (inline gtk-source-view-new))

(defun gtk-source-view-new ()
  (make-instance 'gtk-source-view))

(export 'gtk-source-view-new)


(declaim (inline gtk-source-view-new-with-buffer))

(defun gtk-source-view-new-with-buffer (buffer)
  (make-instance 'gtk-source-view
                 :buffer buffer))

(export 'gtk-source-view-new-with-buffer)


(defcfun ("gtk-source-view-indent-lines" gtk-source-view-indent-lines) :void
  (source-view (g-object gtk-source-view))
  (start (g-object gtk-text-iter))
  (end (g-object gtk-text-iter)))

(export 'gtk-source-view-indent-lines)


(defcfun ("gtk-source-view-unindent-lines" gtk-source-view-unindent-lines) :void
  (source-view (g-object gtk-source-view))
  (start (g-object gtk-text-iter))
  (end (g-object gtk-text-iter)))

(export 'gtk-source-view-unindent-lines)


(defcfun ("gtk-source-view-push-snippet" gtk-source-view-push-snippet) :void
  (source-view (g-object gtk-source-view))
  (snippet (g-object gtk-source-snippet))
  (location (g-object gtk-text-iter)))

(export 'gtk-source-view-unindent-lines)
