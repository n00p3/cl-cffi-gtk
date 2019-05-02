;;; ----------------------------------------------------------------------------
;;; gtk.model-button.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2019 Dieter Kaiser
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
;;; GtkModelButton
;;;
;;;     A button that uses a GAction as model
;;;
;;; Types and Values
;;;
;;;     GtkModelButton
;;;     GtkButtonRole
;;;
;;; Functions
;;;
;;;     gtk_model_button_new ()
;;;
;;; Properties
;;;
;;;          gboolean   active        Read / Write
;;;          gboolean   centered      Read / Write
;;;             GIcon*  icon          Read / Write
;;;          gboolean   iconic        Read / Write
;;;          gboolean   inverted      Read / Write
;;;             gchar*  menu-name     Read / Write
;;;     GtkButtonRole   role          Read / Write
;;;             gchar*  text          Read / Write
;;;          gboolean   use-markup    Read / Write
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkButton
;;;                         ╰── GtkModelButton
;;;
;;; Implemented Interfaces
;;;
;;;     GtkModelButton implements AtkImplementorIface, GtkBuildable,
;;;     GtkActionable and GtkActivatable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkButtonRole
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkButtonRole" gtk-button-role
  (:export t
   :type-initializer "gtk_button_role_get_type")
  :normal
  :check
  :radio)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-button-role atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-button-role atdoc:*external-symbols*)
 "@version{2019-4-2}
  @begin{short}
    The role specifies the desired appearance of a @class{gtk-model-button}.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkButtonRole\" gtk-button-role
  (:export t
   :type-initializer \"gtk_button_role_get_type\")
  :normal
  :check
  :radio)
  @end{pre}
  @begin[code]{table}
    @entry[:normal]{A plain button.}
    @entry[:check]{A check button.}
    @entry[:radio]{A radio button.}
  @end{table}
  @see-class{gtk-model-button}")

;;; ----------------------------------------------------------------------------
;;; struct GtkModelButton
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkModelButton" gtk-model-button
  (:superclass gtk-button
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkActionable"
                "GtkActivatable")
   :type-initializer "gtk_model_button_get_type")
  ((active
    gtk-model-button-active
    "active" "gboolean" t t)
   (centered
    gtk-model-button-centered
    "centered" "gboolean" t t)
   (icon
    gtk-model-button-icon
    "icon" "GIcon" t t)
   (iconic
    gtk-model-button-iconic
    "iconic" "gboolean" t t)
   (inverted
    gtk-model-button-inverted
    "inverted" "gboolean" t t)
   (menu-name
    gtk-model-menu-menu-name
    "menu-name" "gchararray" t t)
   (role
    gtk-model-menu-role
    "role" "GtkButtonRole" t t)
   (text
    gtk-model-button-text
    "text" "gchararray" t t)
   #+gtk-3-24
   (use-markup
    gtk-model-button-use-markup
    "use-markup" "gboolean" t t)
   ))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-model-button 'type)
 "@version{2019-4-2}
  @begin{short}
    @sym{gtk-model-button} is a button class that can use a @class{g-action} as
    its model.
  @end{short}
  In contrast to @class{gtk-toggle-button} or @class{gtk-radio-button}, which
  can also be backed by a @class{g-action} via the @code{action-name} property,
  @sym{gtk-model-button} will adapt its appearance according to the kind of
  action it is backed by, and appear either as a plain, check or radio button.

  Model buttons are used when popovers from a menu model with the function
  @fun{gtk-popover-new-from-model}; they can also be used manually in a
  @class{gtk-popover-menu}.

  When the action is specified via the @code{action-name} and
  @code{action-target} properties, the role of the button, i. e. whether it is
  a plain, check or radio button, is determined by the type of the action and
  doesn't have to be explicitly specified with the @code{role} property.

  The content of the button is specified by the @code{text} and @code{icon}
  properties.

  The appearance of model buttons can be influenced with the @code{centered}
  and @code{iconic} properties.

  Model buttons have built-in support for submenus in @class{gtk-popover-menu}.
  To make a @sym{gtk-model-button} that opens a submenu when activated, set the
  @code{menu-name} property. To make a button that goes back to the parent menu,
  you should set the @code{inverted} property to place the submenu indicator at
  the opposite side.
  @begin[Example]{dictionary}
    @begin{pre}
<object class=\"GtkPopoverMenu\">
  <child>
    <object class=\"GtkBox\">
      <property name=\"visible\">True</property>
      <property name=\"margin\">10</property>
      <child>
        <object class=\"GtkModelButton\">
          <property name=\"visible\">True</property>
          <property name=\"action-name\">view.cut</property>
          <property name=\"text\" translatable=\"yes\">Cut</property>
        </object>
      </child>
      <child>
        <object class=\"GtkModelButton\">
          <property name=\"visible\">True</property>
          <property name=\"action-name\">view.copy</property>
          <property name=\"text\" translatable=\"yes\">Copy</property>
        </object>
      </child>
      <child>
        <object class=\"GtkModelButton\">
          <property name=\"visible\">True</property>
          <property name=\"action-name\">view.paste</property>
          <property name=\"text\" translatable=\"yes\">Paste</property>
        </object>
      </child>
    </object>
  </child>
</object>
    @end{pre}
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    @begin{pre}
 modelbutton
 ├── <child>
 ╰── check

 modelbutton
 ├── <child>
 ╰── radio

 modelbutton
 ├── <child>
 ╰── arrow
    @end{pre}
    @sym{gtk-model-button} has a main CSS node with name @code{modelbutton}, and
    a subnode, which will have the name @code{check}, @code{radio} or
    @code{arrow}, depending on the role of the button and whether it has a menu
    name set.

    The subnode is positioned before or after the content nodes and gets the
    @code{.left} or @code{.right} style class, depending on where it is located.

    @begin{pre}
 button.model
 ├── <child>
 ╰── check
    @end{pre}
    Iconic model buttons, see the @code{iconic} property, change the name of
    their main node to button and add a @code{.model} style class to it. The
    indicator subnode is invisible in this case.
  @end{dictionary}
  @see-slot{gtk-model-button-active}
  @see-slot{gtk-model-button-centered}
  @see-slot{gtk-model-button-icon}
  @see-slot{gtk-model-button-iconic}
  @see-slot{gtk-model-button-inverted}
  @see-slot{gtk-model-button-menu-name}
  @see-slot{gtk-model-button-role}
  @see-slot{gtk-model-button-text}
  @see-slot{gtk-model-button-use-markup}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-model-button-active ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "active"
                                               'gtk-model-button) 't)
 "The @code{active} property of type @code{:boolean}
  (Read / Write) @br{}
  The state of the button. This is reflecting the state of the associated
  @class{g-action}. @br{}
  Default value: @code{nil} @br{}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-model-button-active atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-model-button-active 'function)
 "@version{2019-4-2}
  @begin{short}
    Accessor of the slot @slot[gtk-model-button]{active} of the
    @class{gtk-model-button} class.
  @end{short}
  @see-class{gtk-model-button}")

;;; --- gtk-model-button-centered ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "centered"
                                               'gtk-model-button) 't)
 "The @code{centered} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether to render the button contents centered instead of left-aligned. This
  property should be set for title-like items. @br{}
  Default value: @code{nil} @br{}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-model-button-centered atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-model-button-centered 'function)
 "@version{2019-4-2}
  @begin{short}
    Accessor of the slot @slot[gtk-model-button]{centered} of the
    @class{gtk-model-button} class.
  @end{short}
  @see-class{gtk-model-button}")

;;; --- gtk-model-button-icon --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "icon"
                                               'gtk-model-button) 't)
 "The @code{icon} property of type @class{g-icon} (Read / Write) @br{}
  A @class{g-icon} that will be used if iconic appearance for the button is
  desired. @br{}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-model-button-icon atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-model-button-icon 'function)
 "@version{2019-4-2}
  @begin{short}
    Accessor of the slot @slot[gtk-model-button]{icon} of the
    @class{gtk-model-button} class.
  @end{short}
  @see-class{gtk-model-button}")

;;; --- gtk-model-button-iconic ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "iconic"
                                               'gtk-model-button) 't)
 "The @code{iconic} property of type @code{:boolean} (Read / Write) @br{}
  If this property is set, the button will show an icon if one is set. If no
  icon is set, the text will be used. This is typically used for horizontal
  sections of linked buttons. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-model-button-iconic atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-model-button-iconic 'function)
 "@version{2019-4-2}
  @begin{short}
    Accessor of the slot @slot[gtk-model-button]{iconic} of the
    @class{gtk-model-button} class.
  @end{short}
  @see-class{gtk-model-button}")

;;; --- gtk-model-button-inverted ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "inverted"
                                               'gtk-model-button) 't)
 "The @code{inverted} property of type @code{:boolean} (Read / Write) @br{}
  Whether to show the submenu indicator at the opposite side than normal. This
  property should be set for model buttons that 'go back' to a parent menu.@br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-model-button-inverted atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-model-button-inverted 'function)
 "@version{2019-4-2}
  @begin{short}
    Accessor of the slot @slot[gtk-model-button]{inverted} of the
    @class{gtk-model-button} class.
  @end{short}
  @see-class{gtk-model-button}")

;;; --- gtk-model-button-menu-name ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "menu-name"
                                               'gtk-model-button) 't)
 "The @code{menu-name} property of type @code{:string} (Read / Write) @br{}
  The name of a submenu to open when the button is activated. If this is set,
  the button should not have an action associated with it.@br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-model-button-menu-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-model-button-menu-name 'function)
 "@version{2019-4-2}
  @begin{short}
    Accessor of the slot @slot[gtk-model-button]{menu-name} of the
    @class{gtk-model-button} class.
  @end{short}
  @see-class{gtk-model-button}")

;;; --- gtk-model-button-role --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "role"
                                               'gtk-model-button) 't)
 "The @code{role} property of type @symbol{gtk-button-role} (Read / Write) @br{}
  Specifies whether the button is a plain, check or radio button. When
  @code{action-name} is set, the role will be determined from the action and
  does not have to be set explicitly. @br{}
  Default value: @code{:normal}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-model-button-role atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-model-button-role 'function)
 "@version{2019-4-2}
  @begin{short}
    Accessor of the slot @slot[gtk-model-button]{role} of the
    @class{gtk-model-button} class.
  @end{short}
  @see-class{gtk-model-button}")

;;; --- gtk-model-button-text --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "text"
                                               'gtk-model-button) 't)
 "The @code{text} property of type @code{:string} (Read / Write) @br{}
  The label for the button. @br{}
  Default value: \"\"")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-model-button-text atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-model-button-text 'function)
 "@version{2019-4-2}
  @begin{short}
    Accessor of the slot @slot[gtk-model-button]{text} of the
    @class{gtk-model-button} class.
  @end{short}
  @see-class{gtk-model-button}")

;;; --- gtk-model-button-use-markup --------------------------------------------

#+(and gtk-3-24 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "use-markup"
                                               'gtk-model-button) 't)
 "The @code{use-markjup} property of type @code{:boolean} (Read / Write) @br{}
  If @em{true}, XML tags in the text of the button are interpreted as by the
  function @fun{pango-parse-markup} to format the enclosed spans of text. If
  @code{nil}, the text will be displayed verbatim. @br{}
  Default value: @code{nil} @br{}
  Since 3.24")

#+(and gtk-3-24 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-model-button-use-markup atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-model-button-use-markup 'function)
 "@version{2019-4-2}
  @begin{short}
    Accessor of the slot @slot[gtk-model-button]{use-markup} of the
    @class{gtk-model-button} class.
  @end{short}

  Since 3.24
  @see-class{gtk-model-button}")

;;; ----------------------------------------------------------------------------
;;; gtk_model_button_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-model-button-new))

(defun gtk-model-button-new ()
 #+cl-cffi-gtk-documentation
 "@version{2019-4-2}
  @return{A new @class{gtk-model-button} widget.}
  @begin{short}
    Creates a new @class{gtk-model-button} widget.
  @end{short}

  Since 3.16
  @see-class{gtk-model-button}"
  (make-instance 'gtk-model-button))

(export 'gtk-model-button-new)

;;; --- End of file gtk-model-button.lisp --------------------------------------
