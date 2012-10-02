;;; ----------------------------------------------------------------------------
;;; gdk.device-manager.lisp
;;; 
;;; The documentation has been copied from the GDK 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org.
;;; 
;;; Copyright (C) 2012 Dieter Kaiser
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
;;; GdkDeviceManager
;;;
;;; Functions for handling input devices
;;;
;;; Synopsis
;;;
;;;     GdkDeviceManager
;;;
;;;     gdk_disable_multidevice
;;;     gdk_device_manager_get_display
;;;     gdk_device_manager_list_devices
;;;     gdk_device_manager_get_client_pointer
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GdkDeviceManager
;;;
;;; Properties
;;;
;;;   "display"                  GdkDisplay*          : Read / Write / Construct
;;;
;;; Signals
;;;
;;;   "device-added"                                  : Run Last
;;;   "device-changed"                                : Run Last
;;;   "device-removed"                                : Run Last
;;;
;;; Description
;;;
;;; In addition to a single pointer and keyboard for user interface input, GDK
;;; contains support for a variety of input devices, including graphics tablets,
;;; touchscreens and multiple pointers/keyboards interacting simultaneously with
;;; the user interface. Such input devices often have additional features, such
;;; as sub-pixel positioning information and additional device-dependent
;;; information.
;;;
;;; In order to query the device hierarchy and be aware of changes in the device
;;; hierarchy (such as virtual devices being created or removed, or physical
;;; devices being plugged or unplugged), GDK provides GdkDeviceManager.
;;;
;;; By default, and if the platform supports it, GDK is aware of multiple
;;; keyboard/pointer pairs and multitouch devices. This behavior can be changed
;;; by calling gdk_disable_multidevice() before gdk_display_open(). There should
;;; rarely be a need to do that though, since GDK defaults to a compatibility
;;; mode in which it will emit just one enter/leave event pair for all devices
;;; on a window. To enable per-device enter/leave events and other multi-pointer
;;; interaction features, gdk_window_set_support_multidevice() must be called on
;;; GdkWindows (or gtk_widget_set_support_multidevice() on widgets). window. See
;;; the gdk_window_set_support_multidevice() documentation for more information.
;;;
;;; On X11, multi-device support is implemented through XInput 2. Unless
;;; gdk_disable_multidevice() is called, the XInput 2 GdkDeviceManager
;;; implementation will be used as the input source. Otherwise either the core
;;; or XInput 1 implementations will be used.
;;;
;;; For simple applications that don't have any special interest in input
;;; devices, the so-called client pointer provides a reasonable approximation to
;;; a simple setup with a single pointer and keyboard. The device that has been
;;; set as the client pointer can be accessed via
;;; gdk_device_manager_get_client_pointer().
;;;
;;; Conceptually, in multidevice mode there are 2 device types. Virtual devices
;;; (or master devices) are represented by the pointer cursors and keyboard foci
;;; that are seen on the screen. Physical devices (or slave devices) represent
;;; the hardware that is controlling the virtual devices, and thus have no
;;; visible cursor on the screen.
;;;
;;; Virtual devices are always paired, so there is a keyboard device for every
;;; pointer device. Associations between devices may be inspected through
;;; gdk_device_get_associated_device().
;;;
;;; There may be several virtual devices, and several physical devices could be
;;; controlling each of these virtual devices. Physical devices may also be
;;; "floating", which means they are not attached to any virtual device.
;;;
;;; Example 3. Master and slave devices
;;;
;;; carlossacarino:~$ xinput list
;;; ⎡ Virtual core pointer                         id=2    [master pointer  (3)]
;;; ⎜   ↳ Virtual core XTEST pointer               id=4    [slave  pointer  (2)]
;;; ⎜   ↳ Wacom ISDv4 E6 Pen stylus                id=10   [slave  pointer  (2)]
;;; ⎜   ↳ Wacom ISDv4 E6 Finger touch              id=11   [slave  pointer  (2)]
;;; ⎜   ↳ SynPS/2 Synaptics TouchPad               id=13   [slave  pointer  (2)]
;;; ⎜   ↳ TPPS/2 IBM TrackPoint                    id=14   [slave  pointer  (2)]
;;; ⎜   ↳ Wacom ISDv4 E6 Pen eraser                id=16   [slave  pointer  (2)]
;;; ⎣ Virtual core keyboard                        id=3    [master keyboard (2)]
;;;     ↳ Virtual core XTEST keyboard              id=5    [slave  keyboard (3)]
;;;     ↳ Power Button                             id=6    [slave  keyboard (3)]
;;;     ↳ Video Bus                                id=7    [slave  keyboard (3)]
;;;     ↳ Sleep Button                             id=8    [slave  keyboard (3)]
;;;     ↳ Integrated Camera                        id=9    [slave  keyboard (3)]
;;;     ↳ AT Translated Set 2 keyboard             id=12   [slave  keyboard (3)]
;;;     ↳ ThinkPad Extra Buttons                   id=15   [slave  keyboard (3)]
;;;
;;; By default, GDK will automatically listen for events coming from all master
;;; devices, setting the GdkDevice for all events coming from input devices.
;;; Events containing device information are GDK_MOTION_NOTIFY,
;;; GDK_BUTTON_PRESS, GDK_2BUTTON_PRESS, GDK_3BUTTON_PRESS, GDK_BUTTON_RELEASE,
;;; GDK_SCROLL, GDK_KEY_PRESS, GDK_KEY_RELEASE, GDK_ENTER_NOTIFY,
;;; GDK_LEAVE_NOTIFY, GDK_FOCUS_CHANGE, GDK_PROXIMITY_IN, GDK_PROXIMITY_OUT,
;;; GDK_DRAG_ENTER, GDK_DRAG_LEAVE, GDK_DRAG_MOTION, GDK_DRAG_STATUS,
;;; GDK_DROP_START, GDK_DROP_FINISHED and GDK_GRAB_BROKEN. When dealing with an
;;; event on a master device, it is possible to get the source (slave) device
;;; that the event originated from via gdk_event_get_source_device().
;;;
;;; In order to listen for events coming from devices other than a virtual
;;; device, gdk_window_set_device_events() must be called. Generally, this
;;; function can be used to modify the event mask for any given device.
;;;
;;; Input devices may also provide additional information besides X/Y. For
;;; example, graphics tablets may also provide pressure and X/Y tilt
;;; information. This information is device-dependent, and may be queried
;;; through gdk_device_get_axis(). In multidevice mode, virtual devices will
;;; change axes in order to always represent the physical device that is routing
;;; events through it. Whenever the physical device changes, the "n-axes"
;;; property will be notified, and gdk_device_list_axes() will return the new
;;; device axes.
;;;
;;; Devices may also have associated keys or macro buttons. Such keys can be
;;; globally set to map into normal X keyboard events. The mapping is set using
;;; gdk_device_set_key().
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "display" property
;;;
;;;   "display"                  GdkDisplay*          : Read / Write / Construct
;;;
;;; Display for the device manager.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "device-added" signal
;;;
;;; void user_function (GdkDeviceManager *device_manager,
;;;                     GdkDevice        *device,
;;;                     gpointer          user_data)           : Run Last
;;;
;;; The ::device-added signal is emitted either when a new master pointer is
;;; created, or when a slave (Hardware) input device is plugged in.
;;;
;;; device_manager :
;;;     the object on which the signal is emitted
;;;
;;; device :
;;;     the newly added GdkDevice.
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "device-changed" signal
;;;
;;; void user_function (GdkDeviceManager *device_manager,
;;;                     GdkDevice        *device,
;;;                     gpointer          user_data)           : Run Last
;;;
;;; The ::device-changed signal is emitted whenever a device has changed in the
;;; hierarchy, either slave devices being disconnected from their master device
;;; or connected to another one, or master devices being added or removed a
;;; slave device.
;;;
;;; If a slave device is detached from all master devices
;;; (gdk_device_get_associated_device() returns NULL), its GdkDeviceType will
;;; change to GDK_DEVICE_TYPE_FLOATING, if it's attached, it will change to
;;; GDK_DEVICE_TYPE_SLAVE.
;;;
;;; device_manager :
;;;     the object on which the signal is emitted
;;;
;;; device :
;;;     the GdkDevice that changed.
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "device-removed" signal
;;;
;;; void user_function (GdkDeviceManager *device_manager,
;;;                     GdkDevice        *device,
;;;                     gpointer          user_data)           : Run Last
;;;
;;; The ::device-removed signal is emitted either when a master pointer is
;;; removed, or when a slave (Hardware) input device is unplugged.
;;;
;;; device_manager :
;;;     the object on which the signal is emitted
;;;
;;; device :
;;;     the just removed GdkDevice.
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkDeviceManager
;;;
;;; typedef struct _GdkDeviceManager GdkDeviceManager;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkDeviceManager" gdk-device-manager
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gdk_device_manager_get_type")
  ((display
    gdk-device-manager-display
    "display" "GdkDisplay" t t)))

;;; ----------------------------------------------------------------------------

#-windows
(define-g-object-class "GdkX11DeviceManagerCore" gdk-x11-device-manager-core
  (:superclass gdk-device-manager
   :export t
   :interfaces nil
   :type-initializer "gdk_x11_device_manager_core_get_type")
  nil)

#-windows
(define-g-object-class "GdkX11DeviceManagerXI2" gdk-x11-device-manager-xi2
  (:superclass gdk-x11-device-manager-core
   :export t
   :interfaces nil
   :type-initializer "gdk_x11_device_manager_xi2_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; gdk_disable_multidevice ()
;;;
;;; void gdk_disable_multidevice (void);
;;;
;;; Disables multidevice support in GDK. This call must happen prior to
;;; gdk_display_open(), gtk_init(), gtk_init_with_args() or gtk_init_check() in
;;; order to take effect.
;;;
;;; Most common GTK+ applications won't ever need to call this. Only
;;; applications that do mixed GDK/Xlib calls could want to disable multidevice
;;; support if such Xlib code deals with input devices in any way and doesn't
;;; observe the presence of XInput 2.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_device_manager_get_display ()
;;;
;;; GdkDisplay * gdk_device_manager_get_display
;;;                                          (GdkDeviceManager *device_manager);
;;;
;;; Gets the GdkDisplay associated to device_manager.
;;;
;;; device_manager :
;;;     a GdkDeviceManager
;;;
;;; Returns :
;;;     the GdkDisplay to which device_manager is associated to, or NULL. This
;;;     memory is owned by GDK and must not be freed or unreferenced.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-device-manager-get-display))

(defun gdk-device-manager-get-display (device-manager)
  (gdk-device-manager-display device-manager))

(export 'gdk-device-manager-get-display)

;;; ----------------------------------------------------------------------------
;;; gdk_device_manager_list_devices ()
;;;
;;; GList * gdk_device_manager_list_devices (GdkDeviceManager *device_manager,
;;;                                          GdkDeviceType type);
;;;
;;; Returns the list of devices of type type currently attached to
;;; device_manager.
;;;
;;; device_manager :
;;;     a GdkDeviceManager
;;;
;;; type :
;;;     device type to get.
;;;
;;; Returns :
;;;     a list of GdkDevices. The returned list must be freed with
;;;     g_list_free(). The list elements are owned by GTK+ and must not be freed
;;;     or unreffed.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_device_manager_list_devices" gdk-device-manager-list-devices)
    (g-list (g-object gdk-device))
  (device-manager (g-object gdk-device-manager))
  (type gdk-device-type))

(export 'gdk-device-manager-list-devices)

;;; ----------------------------------------------------------------------------
;;; gdk_device_manager_get_client_pointer ()
;;;
;;; GdkDevice * gdk_device_manager_get_client_pointer
;;;                                          (GdkDeviceManager *device_manager);
;;;
;;; Returns the client pointer, that is, the master pointer that acts as the
;;; core pointer for this application. In X11, window managers may change this
;;; depending on the interaction pattern under the presence of several pointers.
;;;
;;; You should use this function sheldomly, only in code that isn't triggered by
;;; a GdkEvent and there aren't other means to get a meaningful GdkDevice to
;;; operate on.
;;;
;;; device_manager :
;;;     a GdkDeviceManager
;;;
;;; Returns :
;;;     The client pointer. This memory is owned by GDK and must not be freed or
;;;     unreferenced.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_device_manager_get_client_pointer"
           gdk-device-manager-get-client-pointer) (g-object gdk-device)
  (device-manager (g-object gdk-device-manager)))

(export 'gdk-device-manager-get-client-pointer)

;;; --- End of file gdk.device-manager.lisp ------------------------------------
