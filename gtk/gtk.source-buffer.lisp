;;; ----------------------------------------------------------------------------
;;; gtk.source-buffer.lisp
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

(defconstant +GTK-SOURCE-CHANGE-CASE-LOWER+ 0)
(defconstant +GTK-SOURCE-CHANGE-CASE-UPPER+ 1)
(defconstant +GTK-SOURCE-CHANGE-CASE-TOGGLE+ 2)
(defconstant +GTK-SOURCE-CHANGE-CASE-TITLE+ 3)

(export '+GTK-SOURCE-CHANGE-CASE-LOWER+)
(export '+GTK-SOURCE-CHANGE-CASE-UPPER+)
(export '+GTK-SOURCE-CHANGE-CASE-TOGGLE+)
(export '+GTK-SOURCE-CHANGE-CASE-TITLE+)


(defconstant +GTK_SOURCE_SORT_FLAGS_NONE+ 0)
(defconstant +GTK_SOURCE_SORT_FLAGS_CASE_SENSITIVE+ 1)
(defconstant +GTK_SOURCE_SORT_FLAGS_REVERSE_ORDER+ 2)
(defconstant +GTK_SOURCE_SORT_FLAGS_REMOVE_DUPLICATES+ 3)

(export '+GTK_SOURCE_SORT_FLAGS_NONE+)
(export '+GTK_SOURCE_SORT_FLAGS_CASE_SENSITIVE+)
(export '+GTK_SOURCE_SORT_FLAGS_REVERSE_ORDER+)
(export '+GTK_SOURCE_SORT_FLAGS_REMOVE_DUPLICATES+)


(define-g-object-class "GtkSourceBuffer" gtk-source-buffer
  (:superclass gtk-text-buffer
    :export t
   :interfaces nil
   :type-initializer "gtk_source_buffer_get_type")
    ((highlight-syntax
      gtk-source-buffer-highlight-syntax
      "highlight-syntax" "gboolean" t t)
     (highlight-matching-brackets
      gtk-source-buffer-highlight-matching-brackets
      "highlight-matching-brackets" "gboolean" t t)
     (language
      gtk-source-buffer-language
      "language" "gboolean" t t)
     (style-scheme
      gtk-source-buffer-style-scheme
      "style-scheme" "gboolean" t t)
     (source-marks-at-iter
      gtk-source-buffer-source-marks-at-iter
      "source-marks-at-iter" "gboolean" t t)
     (source-marks-at-line
      gtk-source-buffer-source-marks-at-line
      "source-marks-at-line" "gboolean" t t)
     (context-classes-at-iter
      gtk-source-buffer-context-classes-at-iter
      "context-classes-at-iter" "gboolean" t t)
     (implicit-trailing-newline
      gtk-source-buffer-implicit-trailing-newline
      "implicit-trailing-newline" "gboolean" t t)))


(declaim (inline gtk-source-buffer-new))

(defun gtk-source-buffer-new ()
  (make-instance 'gtk-source-buffer-view))

(export 'gtk-source-buffer-new)


(declaim (inline gtk-source-buffer-new-with-language))

(defun gtk-source-buffer-new-with-language (language)
  (make-instance 'gtk-source-buffer
                 :language language))

(export 'gtk-source-buffer-with-language)


(defcfun ("gtk-source-buffer-ensure-highlight" gtk-source-buffer-ensure-highlight) :void
  (source-buffer (g-object gtk-source-buffer))
  (start (g-object gtk-text-iter))
  (end (g-object gtk-text-iter)))

(export 'gtk-source-buffer-ensure-highlight)


(defcfun ("gtk-source-buffer-create-source-mark" gtk-source-buffer-create-source-mark) :void
  (source-buffer (g-object gtk-source-buffer))
  (name (:pointer :char))
  (category (:pointer :char))
  (where (g-object gtk-text-iter)))

(export 'gtk-source-buffer-create-source-mark)


(defcfun ("gtk-source-buffer-forward-iter-to-source-mark" gtk-source-buffer-forward-iter-to-source-mark) :boolean
  (source-buffer (g-object gtk-source-buffer))
  (iter (g-object gtk-text-iter))
  (category (:pointer :char)))

(export 'gtk-source-buffer-forward-iter-to-source-mark)


(defcfun ("gtk-source-buffer-backward-iter-to-source-mark" gtk-source-buffer-backward-iter-to-source-mark) :boolean
  (source-buffer (g-object gtk-source-buffer))
  (iter (g-object gtk-text-iter))
  (category (:pointer :char)))

(export 'gtk-source-buffer-backward-iter-to-source-mark)


(defcfun ("gtk-source-buffer-remove-source-marks" gtk-source-buffer-remove-source-marks) :void
  (source-buffer (g-object gtk-source-buffer))
  (start (g-object gtk-text-iter))
  (end (g-object gtk-text-iter))
  (category (:pointer :char)))

(export 'gtk-source-buffer-remove-source-marks)


(defcfun ("gtk-source-buffer-iter-has-context-class" gtk-source-buffer-iter-has-context-class) :boolean
  (source-buffer (g-object gtk-source-buffer))
  (iter (g-object gtk-text-iter))
  (context-class (:pointer :char)))

(export 'gtk-source-buffer-iter-has-context-class)


(defcfun ("gtk-source-buffer-iter-forward-to-context-class-toggle" gtk-source-buffer-iter-forward-to-context-class-toggle) :boolean
  (source-buffer (g-object gtk-source-buffer))
  (iter (g-object gtk-text-iter))
  (context-class (:pointer :char)))

(export 'gtk-source-buffer-iter-forward-to-context-class-toggle)


(defcfun ("gtk-source-buffer-iter-backward-to-context-class-toggle" gtk-source-buffer-iter-backward-to-context-class-toggle) :boolean
  (source-buffer (g-object gtk-source-buffer))
  (iter (g-object gtk-text-iter))
  (context-class (:pointer :char)))

(export 'gtk-source-buffer-iter-backward-to-context-class-toggle)


(defcfun ("gtk-source-buffer-change-case" gtk-source-buffer-change-case) :void
  (source-buffer (g-object gtk-source-buffer))
  (case-type :int)
  (start (g-object gtk-text-iter))
  (end (g-object gtk-text-iter)))

(export 'gtk-source-buffer-change-case)


(defcfun ("gtk-source-buffer-join-lines" gtk-source-buffer-join-lines) :void
  (source-buffer (g-object gtk-source-buffer))
  (start (g-object gtk-text-iter))
  (end (g-object gtk-text-iter)))

(export 'gtk-source-buffer-join-lines)


(defcfun ("gtk-source-buffer-sort-lines" gtk-source-buffer-sort-lines) :void
  (source-buffer (g-object gtk-source-buffer))
  (start (g-object gtk-text-iter))
  (end (g-object gtk-text-iter))
  (flags :int))

(export 'gtk-source-buffer-sort-lines)

(defcfun ("gtk-source-buffer-create-source-tag" gtk-source-buffer-create-source-tag) (g-object gtk-text-tag)
  (source-buffer (g-object gtk-source-buffer))
  (tag-name (:pointer :char))
  (first-property-name (:pointer :char)))

(export 'gtk-source-buffer-create-source-tag)
