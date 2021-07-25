;;; ----------------------------------------------------------------------------
;;; gtk.statusbar.lisp
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
;;; GtkStatusbar
;;;
;;;     Report messages of minor importance to the user
;;;
;;; Types and Values
;;;
;;;     GtkStatusbar
;;;
;;; Functions
;;;
;;;     gtk_statusbar_new
;;;     gtk_statusbar_get_context_id
;;;     gtk_statusbar_push
;;;     gtk_statusbar_pop
;;;     gtk_statusbar_remove
;;;     gtk_statusbar_remove_all
;;;     gtk_statusbar_get_message_area
;;;
;;; Style Properties
;;;
;;;     GtkShadowType  shadow-type    Read
;;;
;;; Signals
;;;
;;;     void  text-popped    Run Last
;;;     void  text-pushed    Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBox
;;;                     ╰── GtkStatusbar
;;;
;;; Implemented Interfaces
;;;
;;;     GtkStatusbar implements AtkImplementorIface, GtkBuildable and
;;;     GtkOrientable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkStatusbar
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkStatusbar" gtk-statusbar
  (:superclass gtk-box
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable")
   :type-initializer "gtk_statusbar_get_type")
 nil)

(setf (documentation 'gtk-statusbar 'type)
 "@version{*2021-7-24}
  @begin{short}
    A @sym{gtk-statusbar} widget is usually placed along the bottom of the
    main @class{gtk-window} widget of the application. It may provide a regular
    commentary of the status of the application as is usually the case in a web
    browser, for example, or may be used to simply output a message when the
    status changes, when an upload is complete in an FTP client, for example.
  @end{short}

  @image[statusbar]{}

  Status bars in GTK maintain a stack of messages. The message at the top of
  the stack of the status bar is the one that will currently be displayed.

  Any messages added to the stack of the status bar must specify a context ID
  that is used to uniquely identify the source of a message. This context ID can
  be generated by the function @fun{gtk-statusbar-context-id}, given a message
  and the status bar that it will be added to. Note that messages are stored in
  a stack, and when choosing which message to display, the stack structure is
  adhered to, regardless of the context identifier of a message.

  One could say that a status bar maintains one stack of messages for display
  purposes, but allows multiple message producers to maintain sub-stacks of
  the messages they produced (via context IDs).

  Status bars are created using the function @fun{gtk-statusbar-new}.

  Messages are added to the stack of the status bar with the function
  @fun{gtk-statusbar-push}.

  The message at the top of the stack can be removed using the function
  @fun{gtk-statusbar-pop}. A message can be removed from anywhere in the stack
  if its message ID was recorded at the time it was added. This is done using
  the function @fun{gtk-statusbar-remove}.
  @begin[CSS node]{dictionary}
    The @sym{gtk-statusbar} widget has a single CSS node with name
    @code{statusbar}.
  @end{dictionary}
  @begin[Style Property Details]{dictionary}
    @begin[code]{table}
      @begin[shadow-type]{entry}
        The @code{shadow-type} style property of type @symbol{gtk-shadow-type}
        (Read) @br{}
        Style of bevel around the text of the status bar. @br{}
        @em{Warning:} The @code{shadow-type} style property has been deprecated
        since version 3.20 and should not be used in newly written code. Use CSS
        properties to determine the appearance, the value of this style property
        is ignored. @br{}
        Default value: @code{:in}
      @end{entry}
    @end{table}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"text-popped\" signal}
      @begin{pre}
 lambda (statusbar context-id text)    :run-last
      @end{pre}
      Is emitted whenever a new message is popped off the stack of the status
      bar.
      @begin[code]{table}
        @entry[statusbar]{The @sym{gtk-statusbar} widget which received the
          signal.}
        @entry[context-id]{An unsigned integer with the context ID of the
          relevant message/status bar.}
        @entry[text]{A string with the message that was just popped.}
      @end{table}
    @subheading{The \"text-pushed\" signal}
      @begin{pre}
 lambda (statusbar context-id text)    :run-last
      @end{pre}
      Is emitted whenever a new message gets pushed onto the stack of the status
      bar.
      @begin[code]{table}
        @entry[statusbar]{The @sym{gtk-statusbar} widget which received the
          signal.}
        @entry[context-id]{An unsigned integer with the context ID of the
          relevant message/status bar.}
        @entry[text]{A string with the message that was pushed.}
      @end{table}
  @end{dictionary}")

;;; ----------------------------------------------------------------------------
;;; gtk_statusbar_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-statusbar-new))

(defun gtk-statusbar-new ()
 #+cl-cffi-gtk-documentation
 "@version{2020-11-19}
  @return{The new @class{gtk-statusbar} widget.}
  Creates a new status bar ready for messages.
  @see-class{gtk-statusbar}"
  (make-instance 'gtk-statusbar))

(export 'gtk-statusbar-new)

;;; ----------------------------------------------------------------------------
;;; gtk_statusbar_get_context_id () -> gtk-statusbar-context-id
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_statusbar_get_context_id" %gtk-statusbar-get-context-id) :uint
  (statusbar (g-object gtk-statusbar))
  (context-description g-string))

(defun gtk-statusbar-context-id (statusbar context)
 #+cl-cffi-gtk-documentation
 "@version{2020-4-19}
  @argument[statusbar]{a @class{gtk-statusbar} widget}
  @argument[context]{a string with the textual description of what context the
    new message is being used in}
  @return{An integer ID.}
  @begin{short}
    Returns a new context identifier, given a description of the actual context.
  @end{short}
  Note that the description is not shown in the UI.
  @see-class{gtk-statusbar}"
  (etypecase context
    (integer context)
    (string (%gtk-statusbar-get-context-id statusbar context))))

(export 'gtk-statusbar-context-id)

;;; ----------------------------------------------------------------------------
;;; gtk_statusbar_push ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_statusbar_push" %gtk-statusbar-push) :uint
  (statusbar (g-object gtk-statusbar))
  (context-id :uint)
  (text g-string))

(defun gtk-statusbar-push (statusbar context-id text)
 #+cl-cffi-gtk-documentation
 "@version{*2021-7-24}
  @argument[statusbar]{a @class{gtk-statusbar} widget}
  @argument[context-id]{the context ID of the message, as returned by the
    function @fun{gtk-statusbar-context-id}}
  @argument[text]{a string with the message to add to @arg{statusbar}}
  @return{An unsigned integer with a message ID that can be used with the
    function @fun{gtk-statusbar-remove}.}
  @begin{short}
    Pushes a new message onto the stack of the status bar.
  @end{short}
  @see-class{gtk-statusbar}
  @see-function{gtk-statusbar-remove}
  @see-function{gtk-statusbar-context-id}"
  (%gtk-statusbar-push statusbar context-id text))

(export 'gtk-statusbar-push)

;;; ----------------------------------------------------------------------------
;;; gtk_statusbar_pop ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_statusbar_pop" %gtk-statusbar-pop) :void
  (statusbar (g-object gtk-statusbar))
  (context-id :uint))

(defun gtk-statusbar-pop (statusbar context-id)
 #+cl-cffi-gtk-documentation
 "@version{*2021-7-24}
  @argument[statusbar]{a @class{gtk-statusbar} widget}
  @argument[context-id]{an unsigned integer with a context identifier}
  @begin{short}
    Removes the first message in the stack of the status bar with the given
    context ID.
  @end{short}

  Note that this may not change the displayed message, if the message at the
  top of the stack has a different context ID.
  @see-class{gtk-statusbar}
  @see-function{gtk-statusbar-push}"
  (%gtk-statusbar-pop statusbar
                      (gtk-statusbar-context-id statusbar context-id)))

(export 'gtk-statusbar-pop)

;;; ----------------------------------------------------------------------------
;;; gtk_statusbar_remove ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_statusbar_remove" %gtk-statusbar-remove) :void
  (statusbar (g-object gtk-statusbar))
  (context-id :uint)
  (message-id :uint))

(defun gtk-statusbar-remove (statusbar context-id message-id)
 #+cl-cffi-gtk-documentation
 "@version{2020-4-19}
  @argument[statusbar]{a @class{gtk-statusbar} widget}
  @argument[context-id]{an integer with a context identifier}
  @argument[message-id]{an integer with a message identifier, as returned by the
    function @fun{gtk-statusbar-push}}
  @begin{short}
    Forces the removal of a message from a statusbar's stack.
  @end{short}
  The exact @arg{context-id} and @arg{message-id} must be specified.
  @see-class{gtk-statusbar}
  @see-function{gtk-statusbar-push}"
  (%gtk-statusbar-remove statusbar
                         (gtk-statusbar-context-id statusbar context-id)
                         message-id))

(export 'gtk-statusbar-remove)

;;; ----------------------------------------------------------------------------
;;; gtk_statusbar_remove_all ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_statusbar_remove_all" %gtk-statusbar-remove-all) :void
  (statusbar (g-object gtk-statusbar))
  (conext-id :uint))

(defun gtk-statusbar-remove-all (statusbar context)
 #+cl-cffi-gtk-documentation
 "@version{2020-4-19}
  @argument[statusbar]{a @class{gtk-statusbar} widget}
  @argument[context-id]{an integer with a context identifier}
  @begin{short}
    Forces the removal of all messages from a statusbar's stack with the exact
    @arg{context-id}.
  @end{short}
  @see-class{gtk-statusbar}"
  (%gtk-statusbar-remove-all statusbar
                             (gtk-statusbar-context-id statusbar context)))

(export 'gtk-statusbar-remove-all)

;;; ----------------------------------------------------------------------------
;;; gtk_statusbar_get_message_area () -> gtk-statusbar-message-area
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_statusbar_get_message_area" gtk-statusbar-message-area)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2020-4-19}
  @argument[statusbar]{a @class{gtk-statusbar} widget.}
  @return{A @class{gtk-box} widget.}
  @begin{short}
    Retrieves the box containing the label widget.
  @end{short}
  @see-class{gtk-statusbar}"
  (statusbar (g-object gtk-statusbar)))

(export 'gtk-statusbar-message-area)

;;; --- End of file gtk.statusbar.lisp -----------------------------------------
