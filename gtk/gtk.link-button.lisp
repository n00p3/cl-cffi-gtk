;;; ----------------------------------------------------------------------------
;;; gtk.link-button.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2021 Dieter Kaiser
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
;;;
;;; GtkLinkButton
;;;
;;;     Create buttons bound to a URL
;;;
;;; Types and Values
;;;
;;;     GtkLinkButton
;;;
;;; Functions
;;;
;;;     gtk_link_button_new
;;;     gtk_link_button_new_with_label
;;;     gtk_link_button_get_uri                            Accessor
;;;     gtk_link_button_set_uri                            Accessor
;;;     gtk_link_button_get_visited                        Accessor
;;;     gtk_link_button_set_visited                        Accessor
;;;
;;; Properties
;;;
;;;        gchar*   uri              Read / Write
;;;     gboolean    visited          Read / Write
;;;
;;; Signals
;;;
;;;     gboolean    activate-link    Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkButton
;;;                         ╰── GtkLinkButton
;;;
;;; Implemented Interfaces
;;;
;;;     GtkLinkButton implements AtkImplementorIface, GtkBuildable,
;;;     GtkActionable and GtkActivatable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; Class gtk-link-button
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkLinkButton" gtk-link-button
  (:superclass gtk-button
    :export t
    :interfaces ("AtkImplementorIface"
                 "GtkBuildable"
                 "GtkActionable"
                 "GtkActivatable")
    :type-initializer "gtk_link_button_get_type")
  ((uri
    gtk-link-button-uri
    "uri" "gchararray" t t)
   (visited
    gtk-link-button-visited
    "visited" "gboolean" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-link-button 'type)
 "@version{2020-5-12}
  @begin{short}
    A @sym{gtk-link-button} is a @class{gtk-button} with a hyperlink, similar
    to the one used by web browsers, which triggers an action when clicked. It
    is useful to show quick links to resources.
  @end{short}
  A link button is created by calling either the functions
  @fun{gtk-link-button-new} or @fun{gtk-link-button-new-with-label}. If using
  the former, the URI you pass to the constructor is used as a label for the
  widget.

  @image[link-button]{}

  The URI bound to a @sym{gtk-link-button} can be set specifically or retrieved
  using the slot access function @fun{gtk-link-button-uri}.

  By default, @sym{gtk-link-button} calls the function @fun{gtk-show-uri} when
  the button is clicked. This behaviour can be overridden by connecting to the
  \"activate-link\" signal and returning @em{true} from the signal handler.
  @begin[CSS nodes]{dictionary}
    @sym{gtk-link-button} has a single CSS node with name @code{button}. To
    differentiate it from a plain @class{gtk-button}, it gets the @code{.link}
    style class.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate-link\" signal}
      @begin{pre}
 lambda (button)    : Run Last
      @end{pre}
      The \"activate-link\" signal is emitted each time the link button has been
      clicked. The default handler will call the function @fun{gtk-show-uri}
      with the URI stored inside the @code{uri} property. To override the
      default behavior, you can connect to the \"activate-link\" signal and stop
      the propagation of the signal by returning @em{true} from your handler.
      @begin[code]{table}
        @entry[button]{The @sym{gtk-link-button} widget that emitted the
          signal.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-link-button-uri}
  @see-slot{gtk-link-button-visited}
  @see-class{gtk-button}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-link-button-uri ----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "uri" 'gtk-link-button) 't)
 "The @code{uri} property of type @code{:string} (Read / Write) @br{}
  The URI bound to this button. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-link-button-uri atdoc:*function-name-alias*) "Accessor"
      (documentation 'gtk-link-button-uri 'function)
 "@version{2020-5-12}
  @syntax[]{(gtk-link-button-uri object) => uri}
  @syntax[]{(setf (gtk-link-button-uri object) uri)}
  @argument[object]{a @class{gtk-link-button} widget}
  @argument[uri]{a string with a valid URI}
  @begin{short}
    Accessor of the @slot[gtk-link-button]{uri} slot of the
    @class{gtk-link-button} class.
  @end{short}

  The slot access function @sym{gtk-link-button-uri} retrieves the URI. The
  slot access function @sym{gtk-link-button-uri} sets @arg{uri} as the URI
  where the link button points.

  As a side-effect this unsets the visited state of the button.
  @see-class{gtk-link-button}")

;;; --- gtk-link-button-visited ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "visited" 'gtk-link-button) 't)
 "The @code{visited} property of type @code{:boolean} (Read / Write) @br{}
  The visited state of this button. A visited link is drawn in a different
  color. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-link-button-visited atdoc:*function-name-alias*) "Accessor"
      (documentation 'gtk-link-button-visited 'function)
 "@version{2020-5-12}
  @syntax[]{(gtk-link-button-visited object) => visited}
  @syntax[]{(setf (gtk-link-button-visited object) visited)}
  @argument[object]{a @class{gtk-link-button} widget}
  @argument[visited]{a boolean with the \"visited\" state}
  @begin{short}
    Accessor of the @slot[gtk-link-button]{visited} slot of the
    @class{gtk-link-button} class.
  @end{short}

  The slot access function @sym{gtk-link-button-visited} retrieves the
  \"visited\" state of the URI where the link button points. The slot access
  function @sym{(setf gtk-link-button-visited)} sets the \"visited\" state of
  the URI.

  The button becomes visited when it is clicked. If the URI is changed on the
  button, the visited state is unset again.
  @see-class{gtk-link-button}")

;;; ----------------------------------------------------------------------------
;;; gtk-link-button-new
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-link-button-new))

(defun gtk-link-button-new (uri)
 #+cl-cffi-gtk-documentation
 "@version{*2021-5-20}
  @argument[uri]{a string with a valid URI}
  @return{A new @class{gtk-link-button} widget.}
  @begin{short}
    Creates a new link button with the URI as its text.
  @end{short}
  @see-class{gtk-link-button}"
  (make-instance 'gtk-link-button
                 :uri uri
                 :label uri))

(export 'gtk-link-button-new)

;;; ----------------------------------------------------------------------------
;;; gtk-link-button-new-with-label
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-link-button-new-with-label))

(defun gtk-link-button-new-with-label (uri label)
 #+cl-cffi-gtk-documentation
 "@version{*2021-5-20}
  @argument[uri]{a string with a valid URI}
  @argument[label]{a string with the text of the button}
  @return{A new @class{gtk-link-button} widget.}
  @begin{short}
    Creates a new link button containing a label.
  @end{short}
  @see-class{gtk-link-button}"
  (make-instance 'gtk-link-button
                 :uri uri
                 :label label))

(export 'gtk-link-button-new-with-label)

;;; --- End of file gtk.link-button.lisp ---------------------------------------
