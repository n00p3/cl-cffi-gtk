;;; ----------------------------------------------------------------------------
;;; gtk.source-language.lisp
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

(define-g-object-class "GtkSourceLanguage" gtk-source-language
  (:superclass g-object
   :export t
   :interfaces ()
   :type-initializer "gtk_source_language_get_type")
    ())


(defun gtk-source-language-new ()
  (make-instance 'gtk-source-language))

(export 'gtk-source-language-new)


(defcfun ("gtk_source_language_get_id" gtk-source-language-get-id) :int
  (source-language (g-object gtk-source-language)))

(export 'gtk-source-language-get-id)
