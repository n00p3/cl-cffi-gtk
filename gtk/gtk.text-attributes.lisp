;;; ----------------------------------------------------------------------------
;;; gtk.text-attributes.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2020 - 2021 Dieter Kaiser
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
;;; Types and Values
;;;
;;;     GtkWrapMode                                     <--- gtk.text-view.lisp
;;;     GtkTextAppearance                               <--- gtk.text-tag.lisp
;;;     GtkTextAttributes                               <--- gtk.text-tag.lisp
;;;-----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkWrapMode
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkWrapMode" gtk-wrap-mode
  (:export t
   :type-initializer "gtk_wrap_mode_get_type")
  (:none 0)
  (:char 1)
  (:word 2)
  (:word-char 3))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-wrap-mode atdoc:*symbol-name-alias*)
      "GEnum"
      (gethash 'gtk-wrap-mode atdoc:*external-symbols*)
 "@version{2021-8-19}
  @short{Describes a type of line wrapping.}
  @begin{pre}
(define-g-enum \"GtkWrapMode\" gtk-wrap-mode
  (:export t
   :type-initializer \"gtk_wrap_mode_get_type\")
  (:none 0)
  (:char 1)
  (:word 2)
  (:word-char 3))
  @end{pre}
  @begin[code]{table}
    @entry[:none]{Do not wrap lines, just make the text area wider.}
    @entry[:char]{Wrap text, breaking lines anywhere the cursor can appear
      between characters, usually. If you want to be technical, between
      graphemes, see the function @fun{pango-log-attrs}.}
    @entry[:word]{Wrap text, breaking lines in between words.}
    @entry[:word-char]{Wrap text, breaking lines in between words, or if that
      is not enough, also between graphemes.}
  @end{table}
  @see-class{gtk-text-tag}
  @see-class{gtk-text-view}
  @see-function{pango-log-attrs}")

;;; ----------------------------------------------------------------------------
;;; struct GtkTextAppearance
;;;
;;; struct GtkTextAppearance {
;;;   GdkColor bg_color; /* pixel is taken for underline color */
;;;   GdkColor fg_color; /* pixel is taken for strikethrough color */
;;;
;;;   /* super/subscript rise, can be negative */
;;;   gint rise;
;;;
;;;   guint underline : 4;          /* PangoUnderline */
;;;   guint strikethrough : 1;
;;;
;;;   /* Whether to use background-related values; this is irrelevant for
;;;    * the values struct when in a tag, but is used for the composite
;;;    * values struct; it's true if any of the tags being composited
;;;    * had background stuff set.
;;;    */
;;;   guint draw_bg : 1;
;;;
;;;   /* These are only used when we are actually laying out and rendering
;;;    * a paragraph; not when a GtkTextAppearance is part of a
;;;    * GtkTextAttributes.
;;;    */
;;;   guint inside_selection : 1;
;;;   guint is_text : 1;
;;;
;;;   /* For the sad story of this bit of code, see
;;;    * https://bugzilla.gnome.org/show_bug.cgi?id=711158
;;;    */
;;;   #ifdef __GI_SCANNER__
;;;   /* The scanner should only see the transparent union, so that its
;;;    * content does not vary across architectures.
;;;    */
;;;   union {
;;;     GdkRGBA *rgba[2];
;;; };
;;;
;;; Members
;;;
;;; GdkColor bg_color;
;;;     Background GdkColor.
;;;
;;; GdkColor fg_color;
;;;     Foreground GdkColor.
;;;
;;; gint rise;
;;;     Super/subscript rise, can be negative.
;;;
;;; guint underline : 4;
;;;     PangoUnderline
;;;
;;; guint strikethrough : 1;
;;;     Strikethrough style
;;;
;;; guint draw_bg : 1;
;;;     Whether to use background-related values; this is irrelevant for the
;;;     values struct when in a tag, but is used for the composite values
;;;     struct; it’s true if any of the tags being composited had background
;;      stuff set.
;;;
;;; guint inside_selection : 1;
;;;     This are only used when we are actually laying out and rendering a
;;;     paragraph; not when a GtkTextAppearance is part of a GtkTextAttributes.
;;;
;;; guint is_text : 1;
;;;     This are only used when we are actually laying out and rendering a
;;;     paragraph; not when a GtkTextAppearance is part of a GtkTextAttributes.
;;;
;;; GdkRGBA *rgba[2];
;;;     GdkRGBA
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GtkTextAttributes
;;; ----------------------------------------------------------------------------

;; FIXME: This implementation crashes the testsuite, when copying the
;; structure. Work is needed. At this time we do not export the structure.

(glib-init::at-init () (foreign-funcall "gtk_text_attributes_get_type" g-size))

(define-g-boxed-cstruct gtk-text-attributes "GtkTextAttributes"
  (refcount :uint :initform 0) ; private field
  (appearance :pointer :initform (null-pointer))   ; type is gtk-text-appearance
  (justification gtk-justification :initform :left)
  (direction gtk-text-direction :initform :none)
  (font (g-boxed-foreign pango-font-description))
  (font-scale :double :initform 0.0d0)
  (left-margin :int :initform 0)
  (right-margin :int :initform 0)
  (indent :int :initform 0)
  (pixels-above-lines :int :initform 0)
  (pixels-below-lines :int :initform 0)
  (pixels-inside-wrap :int :initform 0)
  (tabs :pointer :initform (null-pointer))             ; type is pango-tab-array
  (wrap-mode gtk-wrap-mode :initform :none)
  (language (g-boxed-foreign pango-language))
  (bg-color (g-boxed-foreign gdk-color)) ; private field
  (invisible :uint :initform 1)
  (bg-full-height :uint :initform 1)
  (editable :uint :initform 1)
  (no-fallback :uint :initform 1)
  (bg-rgba (g-boxed-foreign gdk-rgba)) ; private field
  (letter-spacing :int :initform 0)
  (font-features :pointer :initform (null-pointer)) ; type is char*
  (dummy1 :pointer)
  (dummy2 :pointer))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-attributes atdoc:*class-name-alias*)
      "GBoxed"
      (documentation 'gtk-text-attributes 'type)
 "@version{2021-8-19}
  @begin{short}
    Using the @sym{gtk-text-attributes} structure directly should rarely be
    necessary.
  @end{short}
  It is primarily useful with the function @fun{gtk-text-iter-attributes}. As
  with most GTK structures, the fields in this structure should only be read,
  never modified directly.
  @begin{pre}
(define-g-boxed-cstruct gtk-text-attributes \"GtkTextAttributes\"
  (appearance :pointer :initform (null-pointer))
  (justification gtk-justification)
  (direction gtk-text-direction)
  (font (g-boxed-foreign pango-font-description))
  (font-scale :double)
  (left-margin :int)
  (right-margin :int)
  (indent :int)
  (pixels-above-lines :int)
  (pixels-below-lines :int)
  (pixels-inside-wrap :int)
  (tabs :pointer)             ; type is pango-tab-array
  (wrap-mode gtk-wrap-mode)
  (language (g-boxed-foreign pango-language))
  (invisible :uint)
  (bg-full-height :uint)
  (editable :uint)
  (no-fallback :uint)
  (letter-spacing :int)
  (font-features :string))
  @end{pre}
  @begin[code]{table}
    @entry[appearance]{Pointer to a @code{GtkTextAppearance} structure for
      text.}
    @entry[justification]{A value of the @symbol{gtk-justification} enumeration
      for text.}
    @entry[direction]{A value of the @symbol{gtk-text-direction} enumeration
      for text.}
    @entry[font]{The @class{pango-font-description} structure for text.}
    @entry[font-scale]{A double float with the font scale factor.}
    @entry[left-margin]{An integer with the width of the left margin,
      in pixels.}
    @entry[right-margin]{An integer with the width of the right margin,
      in pixels.}
    @entry[indent]{An integer with the amount to indent the paragraph,
      in pixels.}
    @entry[pixels-above-lines]{An integer with the pixels of blank space above
      paragraphs.}
    @entry[pixels-below-lines]{An integer with the pixels of blank space below
      paragraphs.}
    @entry[pixels-inside-wrap]{An integer with the pixels of blank space
      between wrapped lines in a paragraph.}
    @entry[tabs]{Pointer to a custom @class{pango-tab-array} structure for
      this text.}
    @entry[wrap-mode]{A value of the @symbol{gtk-wrap-mode} enumeration for
      text.}
    @entry[language]{Pointer to a @class{pango-language} structure for text.}
    @entry[invisible]{An unsigned integer whether to hide the text.}
    @entry[bg-full-height]{Whether background is fit to full line height rather
      than baseline +/- ascent/descent (font height).}
    @entry[editable]{Whether the text is editable.}
    @entry[no-fallback]{Whether to disable font fallback.}
    @entry[letter-spacing]{An integer with the extra space to insert between
      graphemes, in Pango units.}
    @entry[font-features]{A string with font features.}
  @end{table}
  @see-slot{gtk-text-attributes-appearance}
  @see-slot{gtk-text-attributes-justification}
  @see-slot{gtk-text-attributes-direction}
  @see-slot{gtk-text-attributes-font}
  @see-slot{gtk-text-attributes-font-scale}
  @see-slot{gtk-text-attributes-left-margin}
  @see-slot{gtk-text-attributes-right-margin}
  @see-slot{gtk-text-attributes-indent}
  @see-slot{gtk-text-attributes-pixels-above-lines}
  @see-slot{gtk-text-attributes-pixels-below-lines}
  @see-slot{gtk-text-attributes-pixels-inside-wrap}
  @see-slot{gtk-text-attributes-tabs}
  @see-slot{gtk-text-attributes-wrap-mode}
  @see-slot{gtk-text-attributes-language}
  @see-slot{gtk-text-attributes-invisible}
  @see-slot{gtk-text-attributes-bg-full-height}
  @see-slot{gtk-text-attributes-editable}
  @see-slot{gtk-text-attributes-no-fallback}
  @see-slot{gtk-text-attributes-letter-spacing}
  @see-slot{gtk-text-attributes-font-features}
  @see-function{gtk-text-iter-attributes}")

;(export (boxed-related-symbols 'gtk-text-attributes))

;; Unexport the private field of the GtkTextAttributes structure
(unexport 'gtk-text-attributes-refcount)
(unexport 'gtk-text-attributes-bg-color)
(unexport 'gtk-text-attributes-bg-rgba)

;;; ----------------------------------------------------------------------------
;;; Constructors for GtkTextAttributes                     not exported
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'make-gtk-text-attributes 'function)
 "@version{2021-8-19}
  @begin{short}
    Creates and returns a structure of type @class{gtk-text-attributes}.
  @end{short}
  @see-class{gtk-text-attributes}
  @see-function{copy-gtk-text-attributes}")

(unexport 'make-gtk-text-attributes)

#+cl-cffi-gtk-documentation
(setf (documentation 'copy-gtk-text-attributes 'function)
 "@version{2021-8-19}
  @begin{short}
    Copies and returns a structure of type @class{gtk-text-attributes}.
  @end{short}
  @see-class{gtk-text-attributes}
  @see-function{make-gtk-text-attributes}")

(unexport 'copy-gtk-text-attributes)

;;; ----------------------------------------------------------------------------
;;; Accessors for GtkTextAttributes                        not exported
;;; ----------------------------------------------------------------------------

;;; --- gtk-text-attributes-appearance -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-attributes-appearance atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-attributes-appearance 'function)
 "@version{2021-8-19}
  @begin{short}
    Accessor of the @code{appearance} slot of the @class{gtk-text-attributes}
    structure.
  @end{short}
  Pointer to a @code{GtkTextAppearance} structure for text.
  @see-class{gtk-text-attributes}")

;;; --- gtk-text-attributes-justification --------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-attributes-justification atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-attributes-justification 'function)
 "@version{2021-8-19}
  @begin{short}
    Accessor of the @code{justification} slot of the @class{gtk-text-attributes}
    structure.
  @end{short}
  A value of the @symbol{gtk-justification} enumeration for text.
  @see-class{gtk-text-attributes}
  @see-symbol{gtk-justification}")

;;; --- gtk-text-attributes-direction ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-attributes-direction atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-attributes-direction 'function)
 "@version{2021-8-19}
  @begin{short}
    Accessor of the @code{direction} slot of the @class{gtk-text-attributes}
    structure.
  @end{short}
  A value of the @symbol{gtk-text-direction} enumeration for text.
  @see-class{gtk-text-attributes}
  @see-symbol{gtk-text-direction}")

;;; --- gtk-text-attributes-font -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-attributes-font atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-attributes-font 'function)
 "@version{2021-8-19}
  @begin{short}
    Accessor of the @code{font} slot of the @class{gtk-text-attributes}
    structure.
  @end{short}
  The @class{pango-font-description} structure for text.
  @see-class{gtk-text-attributes}
  @see-class{pango-font-description}")

;;; --- gtk-text-attributes-font-scale -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-attributes-font-scale atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-attributes-font-scale 'function)
 "@version{2021-8-19}
  @begin{short}
    Accessor of the @code{font-scale} slot of the @class{gtk-text-attributes}
    structure.
  @end{short}
  A double float with the font scale factor.
  @see-class{gtk-text-attributes}")

;;; --- gtk-text-attributes-left-margin ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-attributes-left-margin atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-attributes-left-margin 'function)
 "@version{2021-8-19}
  @begin{short}
    Accessor of the @code{left-margin} slot of the @class{gtk-text-attributes}
    structure.
  @end{short}
  An integer with the width of the left margin, in pixels.
  @see-class{gtk-text-attributes}")

;;; --- gtk-text-attributes-right-margin ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-attributes-right-margin atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-attributes-right-margin 'function)
 "@version{2021-8-19}
  @begin{short}
    Accessor of the @code{right-margin} slot of the @class{gtk-text-attributes}
    structure.
  @end{short}
  An integer with the width of the right margin, in pixels.
  @see-class{gtk-text-attributes}")

;;; --- gtk-text-attributes-indent ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-attributes-indent atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-attributes-indent 'function)
 "@version{2021-8-19}
  @begin{short}
    Accessor of the @code{indent} slot of the @class{gtk-text-attributes}
    structure.
  @end{short}
  An integer with the amount to indent the paragraph, in pixels.
  @see-class{gtk-text-attributes}")

;;; --- gtk-text-attributes-pixels-above-lines ---------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-attributes-pixels-above-lines
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-attributes-pixels-above-lines 'function)
 "@version{2021-8-19}
  @begin{short}
    Accessor of the @code{pixels-above-lines} slot of the
    @class{gtk-text-attributes} structure.
  @end{short}
  An integer with the pixels of blank space above paragraphs.
  @see-class{gtk-text-attributes}")

;;; --- gtk-text-attributes-pixels-below-lines ---------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-attributes-pixels-below-lines
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-attributes-pixels-below-lines 'function)
 "@version{2021-8-19}
  @begin{short}
    Accessor of the @code{pixels-below-lines} slot of the
    @class{gtk-text-attributes} structure.
  @end{short}
  An integer with the pixels of blank space below paragraphs.
  @see-class{gtk-text-attributes}")

;;; --- gtk-text-attributes-pixels-inside-wrap ---------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-attributes-pixels-inside-wrap
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-attributes-pixels-inside-wrap 'function)
 "@version{2021-8-19}
  @begin{short}
    Accessor of the @code{pixels-inside-wrap} slot of the
    @class{gtk-text-attributes} structure.
  @end{short}
  An integer with the pixels of blank space between wrapped lines in a
  paragraph.
  @see-class{gtk-text-attributes}")

;;; --- gtk-text-attributes-tabs -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-attributes-tabs atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-attributes-tabs 'function)
 "@version{2021-8-19}
  @begin{short}
    Accessor of the @code{tabs} slot of the @class{gtk-text-attributes}
    structure.
  @end{short}
  Pointer to a custom @class{pango-tab-array} instance for this text.
  @see-class{gtk-text-attributes}
  @see-class{pango-tab-array}")

;;; --- gtk-text-attributes-wrap-mode ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-attributes-wrap-mode atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-attributes-wrap-mode 'function)
 "@version{2021-8-19}
  @begin{short}
    Accessor of the @code{wrap-mode} slot of the @class{gtk-text-attributes}
    structure.
  @end{short}
  A value of the @symbol{gtk-wrap-mode} enumeration for text.
  @see-class{gtk-text-attributes}
  @see-symbol{gtk-wrap-mode}")

;;; --- gtk-text-attributes-language -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-attributes-language atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-attributes-language 'function)
 "@version{2021-8-19}
  @begin{short}
    Accessor of the @code{language} slot of the @class{gtk-text-attributes}
    structure.
  @end{short}
  Pointer to a @class{pango-language} instance for text.
  @see-class{gtk-text-attributes}
  @see-class{pango-language}")

;;; --- gtk-text-attributes-invisible ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-attributes-invisible atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-attributes-invisible 'function)
 "@version{2021-8-19}
  @begin{short}
    Accessor of the @code{invisible} slot of the @class{gtk-text-attributes}
    structure.
  @end{short}
  An unsigned integer whether to hide the text.
  @see-class{gtk-text-attributes}")

;;; --- gtk-text-attributes-bg-full-height -------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-attributes-bg-full-height atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-attributes-bg-full-height 'function)
 "@version{2021-8-19}
  @begin{short}
    Accessor of the @code{bg-full-height} slot of the
    @class{gtk-text-attributes} structure.
  @end{short}
  Whether background is fit to full line height rather than baseline +/-
  ascent/descent (font height).
  @see-class{gtk-text-attributes}")

;;; --- gtk-text-attributes-editable -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-attributes-editable atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-attributes-editable 'function)
 "@version{2021-8-19}
  @begin{short}
    Accessor of the @code{editable} slot of the @class{gtk-text-attributes}
    structure.
  @end{short}
  Whether the text is editable.
  @see-class{gtk-text-attributes}")

;;; --- gtk-text-attributes-no-fallback ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-attributes-no-fallback atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-attributes-no-fallback 'function)
 "@version{2021-8-19}
  @begin{short}
    Accessor of the @code{no-fallback} slot of the @class{gtk-text-attributes}
    structure.
  @end{short}
  Whether to disable font fallback.
  @see-class{gtk-text-attributes}")

;;; --- gtk-text-attributes-letter-spacing -------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-attributes-letter-spacing atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-attributes-letter-spacing 'function)
 "@version{2021-8-19}
  @begin{short}
    Accessor of the @code{letter-spacing} slot of the
    @class{gtk-text-attributes} structure.
  @end{short}
  An integer with the extra space to insert between graphemes, in Pango units.
  @see-class{gtk-text-attributes}")

;;; --- gtk-text-attributes-font-features --------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-attributes-font-features atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-attributes-font-features 'function)
 "@version{2021-8-19}
  @begin{short}
    Accessor of the @code{font-features} slot of the
    @class{gtk-text-attributes} structure.
  @end{short}
  A string with font features.
  @see-class{gtk-text-attributes}")

;;; ----------------------------------------------------------------------------
;;; gtk_text_attributes_new ()
;;; ----------------------------------------------------------------------------

(defun gtk-text-attributes-new ()
 #+cl-cffi-gtk-documentation
 "@version{2021-8-19}
  @return{A new @class{gtk-text-attributes} instance.}
  @begin{short}
    Creates a @class{gtk-text-attributes} instance, which describes a set of
    properties on some text.
  @end{short}
  @see-class{gtk-text-attributes}"
  (make-gtk-text-attributes))

;;; ----------------------------------------------------------------------------
;;; gtk_text_attributes_copy ()
;;; ----------------------------------------------------------------------------

(defun gtk-text-attributes-copy (src)
 #+cl-cffi-gtk-documentation
 "@version{2021-8-19}
  @argument[src]{a @class{gtk-text-attributes} instance to be copied}
  @return{A copy of @arg{src}.}
  @begin{short}
    Copies @arg{src} and returns a new @class{gtk-text-attributes} instance.
  @end{short}
  @see-class{gtk-text-attributes}"
  (copy-gtk-text-attributes src))

;;; ----------------------------------------------------------------------------
;;; gtk_text_attributes_copy_values ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_attributes_copy_values" gtk-text-attributes-copy-values)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2021-8-19}
  @argument[src]{a @class{gtk-text-attributes} instance}
  @argument[dest]{another @class{gtk-text-attributes} instance}
  @begin{short}
    Copies the values from @arg{src} to @arg{dest} so that @arg{dest} has the
    same values as @arg{src}.
  @end{short}
  @see-class{gtk-text-attributes}"
  (src (g-boxed-foreign gtk-text-attributes))
  (dest (g-boxed-foreign gtk-text-attributes)))

;;; ----------------------------------------------------------------------------
;;; gtk_text_attributes_unref ()
;;;
;;; void gtk_text_attributes_unref (GtkTextAttributes *values);
;;;
;;; Decrements the reference count on values, freeing the structure if the
;;; reference count reaches 0.
;;;
;;; values :
;;;     a GtkTextAttributes
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_attributes_ref ()
;;;
;;; GtkTextAttributes * gtk_text_attributes_ref (GtkTextAttributes *values);
;;;
;;; Increments the reference count on values.
;;;
;;; values :
;;;     a GtkTextAttributes
;;;
;;; Returns :
;;;     the GtkTextAttributes that were passed in
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.text-attributes.lisp -----------------------------------
