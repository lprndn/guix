;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>
;;; Copyright © 2020 L  p R n  d n <guix@lprndn.info>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gnu packages pantheon)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
<<<<<<< HEAD
  #:use-module (gnu packages cmake)
=======
>>>>>>> 723f4bebc2... gnu: Add gala.
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages inkscape)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) :prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public granite
  (package
    (name "granite")
    (version "5.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/elementary/granite.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0acicv3f9gksb352v88lwap8ailjsxdrfknl2xql7blasbjzl2q0"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-icon-cache
           (lambda _
             (setenv "DESTDIR" "/")
             #t)))))
    (inputs
     `(("glib" ,glib)
       ("gtk" ,gtk+)
       ("libgee" ,libgee)))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (home-page "https://github.com/elementary/granite")
    (synopsis "Library that extends GTK with common widgets and utilities")
    (description "Granite is a companion library for GTK+ and GLib.  Among other
things, it provides complex widgets and convenience functions designed for use
in apps built for the Pantheon desktop.")
    (license license:lgpl3+)))

(define-public bamf
  (package
    (name "bamf")
    (version "0.5.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.launchpad.net/~unity-team/bamf")
                    ;; 0.5.4
                    (commit "054fbdf625c4c559f74673662e7b595b144ce468")))
              (sha256 (base32
                       "1klvij1wyhdj5d8sr3b16pfixc1yk8ihglpjykg7zrr1f50jfgsz"))
              (patches (search-patches "bamf-use-python3-lxml.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:configure-flags
       (list "--enable-gtk-doc"
             ;; Glib 2.62. Taken from NixOS
             "CFLAGS=-DGLIB_DISABLE_DEPRECATION_WARNINGS")
       #:make-flags
       (list (string-append "INTROSPECTION_GIRDIR="
                            (assoc-ref %outputs "out")
                            "/share/gir-1.0/")
             (string-append "INTROSPECTION_TYPELIBDIR="
                            (assoc-ref %outputs "out")
                            "/lib/girepository-1.0"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'configure 'fix-hardcoded-paths
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "data/Makefile"
                 (("/usr/lib/systemd/user")
                  (string-append out "/lib/systemd/user")))
               #t))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("gobject-introspection" ,gobject-introspection)
       ("vala" ,vala)
       ("glib:bin" ,glib "bin")
       ("python" ,python-wrapper)
       ("python-lxml" ,python-lxml)
       ("which" ,which)
       ("gnome-common" ,gnome-common)
       ("automake" ,automake)
       ("autoconf" ,autoconf)
       ("libtool" ,libtool)
       ("gtk-doc" ,gtk-doc)))
    (inputs
     `(("libgtop" ,libgtop)
       ("libwnck" ,libwnck)
       ("startup-notification" ,startup-notification)))
    (home-page "https://launchpad.net/bamf")
    (synopsis "Application matching framework")
    (description "Removes the headache of applications matching
 into a simple DBus daemon and c wrapper library.")
    (license (list license:gpl3
                   license:lgpl3
                   license:lgpl2.1))))

(define-public pantheon-calculator
  (package
    (name "pantheon-calculator")
    (version "1.5.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/elementary/calculator.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1csxsr2c8qvl97xz9ahwn91z095nzgr0i1mbcb1spljll2sr9lkj"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-schema-cache-generation
           (lambda _
             (setenv "DESTDIR" "/")
             #t)))))
    (inputs
     `(("granite" ,granite)
       ("glib" ,glib)
       ("gtk" ,gtk+)
       ("libgee" ,libgee)))
    (native-inputs
     `(("cmake" ,cmake)
       ("glib:bin" ,glib "bin") ; for glib-compile-schemas
       ("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (home-page "https://github.com/elementary/calculator")
    (synopsis "Desktop calculator")
    (description "Calculator is an application for performing simple
arithmetic.  It is the default calculator application in the Pantheon
desktop.")
    (license license:gpl3)))

(define-public sideload
  (package
    (name "sideload")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/elementary/sideload.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1nnaq4vc0aag6pckxhrma5qv8al7i00rrlg95ac4iqqmivja7i92"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:configure-flags (list (string-append "-Dflatpak="
                                              (assoc-ref %build-inputs "flatpak")
                                              "/include"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-symlinks
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin/io.elementary.sideload"))
                    (link (string-append out "/bin/sideload")))
               (symlink bin link)))))))
    (inputs
     `(("flatpak" ,flatpak)
       ("granite" ,granite)
       ("gtk" ,gtk+)
       ("libostree" ,libostree)))
    (propagated-inputs
     `(("glib-networking" ,glib-networking)))
    (native-inputs
     `(("cmake" ,cmake)
       ("desktop-file-utils" ,desktop-file-utils) ; for update-desktop-database
       ("gettext" ,gettext-minimal)
       ("glib" ,glib)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("libgee" ,libgee)
       ("libxml2" ,libxml2)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (home-page "https://github.com/elementary/sideload")
    (synopsis "Graphical application to side-load Flatpaks")
    (description "Sideload handles flatpakref files, like those you might find
on Flathub or another third-party website providing a Flatpak app for
download.")
    (license license:gpl3)))

(define-public pantheon-terminal
  (package
    (name "pantheon-terminal")
    (version "5.5.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/elementary/terminal.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "119iwmzbpkj4nmxinqfsh73lx23g8gbl6ha6wc4mc4fq9hpnc9c2"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-symlinks
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin/io.elementary.terminal"))
                    (link (string-append out "/bin/pantheon-terminal")))
               (symlink bin link)))))))
    (inputs
     `(("granite" ,granite)
       ("gtk" ,gtk+)
       ("vte" ,vte)))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib" ,glib)
       ("appstream" ,appstream)
       ("libgee" ,libgee)
       ("desktop-file-utils" ,desktop-file-utils) ; for update-desktop-database
       ("glib:bin" ,glib "bin") ; for glib-compile-resources
       ("pkg-config" ,pkg-config)
       ("gobject-introspection" ,gobject-introspection)
       ("vala" ,vala)))
    (home-page "https://github.com/elementary/terminal")
    (synopsis "Graphical terminal with opinionated design and thoughtful touches")
    (description "A lightweight, beautiful, and simple terminal application.
Comes with sane defaults, browser-like tabs, sudo paste protection, smart
copy/paste, and little to no configuration.")
    (license license:lgpl3)))

(define-public plank
  (package
    (name "plank")
    (version "0.11.89")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://launchpad.net/plank/1.0/" version
                    "/+download/plank-" version ".tar.xz"))
              (sha256 (base32
                       "17cxlmy7n13jp1v8i4abxyx9hylzb39andhz3mk41ggzmrpa8qm6"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:tests? #f
       #:make-flags
       (list (string-append "INTROSPECTION_GIRDIR="
                            (assoc-ref %outputs "out")
                            "/share/gir-1.0/")
             (string-append "INTROSPECTION_TYPELIBDIR="
                            (assoc-ref %outputs "out")
                            "/lib/girepository-1.0"))))
    (native-inputs
     `(("vala" ,vala)
       ("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("libxml2" ,libxml2)
       ("gobject-introspection" ,gobject-introspection)))
    (inputs
     `(("bamf" ,bamf)
       ("cairo" ,cairo)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("glib:bin" ,glib "bin")
       ("gnome-menus" ,gnome-menus)
       ("libdbusmenu" ,libdbusmenu)
       ("gtk+" ,gtk+)
       ("libx11" ,libx11)
       ("libxfixes" ,libxfixes)
       ("libxi" ,libxi)
       ("libgee" ,libgee)
       ("libwnck" ,libwnck)))
    (home-page "https://launchpad.net/plank")
    (synopsis "Simple dock application")
    (description "Simple dock application.  It is, however, a library
 which can be extended to create other dock programs with more advanced features.")
    (license (list license:gpl3+
                   license:lgpl2.1+))))

(define-public elementary-wallpapers
  (package
    (name "elementary-wallpapers")
    (version "5.5.0")
    (source (origin
              (method git-fetch)
              (file-name (git-file-name name version))
              (uri (git-reference
                    (url "https://github.com/elementary/wallpapers.git")
                    (commit "47b25c2268ff011af74628dedc00d66e73fe051e")))
              (sha256 (base32
                       "0c63nds2ylqgcp39s13mfwhipgyw8cirn0bhybp291l5g86ii6s3"))))
    (build-system meson-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)))
    (home-page "https://github.com/elementary/wallpapers")
    (synopsis "Collection of wallpapers from elementary OS")
    (description "Collection of wallpapers from elementary OS.")
    ;; TODO: Add unsplash license
    (license license:public-domain)))

(define-public elementary-gtk-theme
  (package
   (name "elementary-gtk-theme")
   (version "5.4.2")
   (source (origin
             (method url-fetch)
             (uri (string-append "https://github.com/elementary/stylesheet/archive/"
                                version ".tar.gz" ))
            (sha256 (base32
                     "1v7038xa5m47xj41762lcs6zxbsm03pjbhg4kiy0pkplv8ipghrv"))))
   (native-inputs
    `(("gettext" ,gettext-minimal)))
   (build-system meson-build-system)
   (home-page "https://github.com/elementary/stylesheet")
   (synopsis "Gtk Stylesheet for elementary OS")
   (description "An original Gtk.CSS stylesheet designed specifically
for elementary OS and its desktop environment: Pantheon.")
   (license license:gpl3)))

(define-public elementary-icon-theme
  (package
    (name "elementary-icon-theme")
    (version "5.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/elementary/icons/archive/"
                                  version ".tar.gz"))
              (sha256 (base32
                       "04lyqlbyiw5cvlcbqb7jgji26k5g9ipxkf0vaacy74497x2lkcv7"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags
       (list "-Dvolume_icons=false")))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("gettext" ,gettext-minimal)
       ("inkscape" ,inkscape)
       ("xcursorgen" ,xcursorgen)
       ("librsvg" ,librsvg)))
    (inputs
     `(("gtk+" ,gtk+)))
    (propagated-inputs
     `(("hicolor-icon-theme" ,hicolor-icon-theme)))
    (home-page "https://github.com/elementary/icons")
    (synopsis "Named, vector icons for elementary OS")
    (description "An original set of vector icons designed
 specifically for elementary OS and its desktop environment: Pantheon.")
    (license license:gpl3)))

(define-public gala
  (package
    (name "gala")
    (version "3.3.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/elementary/gala/archive/"
                                  version ".tar.gz"))
              (sha256 (base32
                       "14l1qqsl66snbrrkhdfwsrpfm658h3vyq1m3zwpm50gxzn0hmciz"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-plugins-dir
           (lambda _
             (substitute* "meson.build"
               (("'PLUGINDIR', plugins_dir")
                "'PLUGINDIR', '/run/current-system/profile/lib/gala/plugins'")))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("vala" ,vala)
       ("gettext" ,gettext-minimal)
       ("desktop-file-utils" ,desktop-file-utils)
       ("gobject-introspection" ,gobject-introspection)
       ("libxml2" ,libxml2)
       ("glib:bin" ,glib "bin")
       ("gtk+:bin" ,gtk+ "bin")))
    (inputs
     `(("atk" ,atk)
       ("cairo" ,cairo)
       ("glib" ,glib)
       ("wayland" ,wayland)
       ("libxfixes" ,libxfixes)
       ("xorg-server" ,xorg-server)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("json-glib" ,json-glib)
       ("pango" ,pango)
       ;;
       ("bamf" ,bamf)
       ("clutter" ,clutter)
       ("gnome-desktop" ,gnome-desktop)
       ("gnome-settings-daemon" ,gnome-settings-daemon)
       ("granite" ,granite)
       ("gtk+" ,gtk+)
       ("libcanberra" ,libcanberra)
       ("libgee" ,libgee)
       ("mutter" ,mutter)
       ("plank" ,plank)))
    (home-page "https://github.com/elementary/gala")
    (synopsis "Window Manager from elementary OS")
    (description "A window & compositing manager
based on libmutter and designed by elementary for use with Pantheon.")
    (license license:gpl3)))

(define-public wingpanel
  (package
    (name "wingpanel")
    ;; 2.3.1 fails with our gnome 3.34 (at least)
    ;; probably incompatible vapi file
    (version "2.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/elementary/wingpanel/archive/"
                                  version ".tar.gz"))
              (sha256 (base32
                       "1mw36azmzkj9hh6zbwqxlxh8f9l2h4dfdwrk9vkxynxw2a0ayl2a"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-indicators-dir
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "meson.build"
               (("get_option\\('prefix'\\), get_option\\('libdir'\\)")
                "'/run/current-system/profile/lib'"))
             #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("vala" ,vala)
       ("gettext" ,gettext-minimal)
       ("gobject-introspection" ,gobject-introspection)
       ("glib:bin" ,glib "bin")
       ("gala" ,gala)))
    (inputs
     `(("granite" ,granite)
       ("gtk+" ,gtk+)
       ("json-glib" ,json-glib)
       ("libgee" ,libgee)
       ("mutter" ,mutter)))
    (home-page "https://github.com/elementary/wingpanel")
    (synopsis "Top panel that holds indicators and spawns an application launcher")
    (description "The extensible top panel for Pantheon.
It is an empty container that accepts indicators as extensions,
including the applications menu.")
    (license license:gpl3)))

(define-public switchboard
  (package
   (name "switchboard")
   (version "2.4.0")
   (source (origin
             (method url-fetch)
             (uri (string-append "https://github.com/elementary/switchboard/archive/"
                                 version ".tar.gz"))
             (sha256 (base32
                      "1crz8gkw6fqzijw2qf7zq1g0cdyrgpx3j21lm6l8rj7ly8v0nz7m"))))
   (build-system meson-build-system)
   (arguments
    `(#:glib-or-gtk? #t
      #:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'wrap-program
          (lambda* (#:key outputs #:allow-other-keys)
            (substitute* "meson.build"
              (("get_option\\('prefix'\\), get_option\\('libdir'\\)")
               "'/run/current-system/profile/lib'")))))))
   (native-inputs
    `(("pkg-config" ,pkg-config)
      ("vala" ,vala)
      ("glib:bin" ,glib "bin")
      ("gettext" ,gettext-minimal)
      ("gobject-introspection"  ,gobject-introspection)))
   (propagated-inputs
    `(("glib" ,glib)
      ("gtk+" ,gtk+)
      ("libgee" ,libgee)))
   (inputs
    `(("libgee" ,libgee)
      ("gtk+" ,gtk+)
      ("granite" ,granite)
      ("clutter-gtk" ,clutter-gtk)))
   (home-page "https://github.com/elementary/switchboard")
   (synopsis "Extensible System Settings app designed for elementary OS")
   (description "Switchboard is just the container application for Switchboard Plugs,
which provide the actual settings for various hardware and software.")
   (license license:lgpl2.1)))

(define-public wingpanel-indicator-sound
  (package
    (name "wingpanel-indicator-sound")
    (version "2.1.5")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/elementary/wingpanel-indicator-sound/archive/"
                    version ".tar.gz"))
              (sha256 (base32
                       "05i45znsx2s4dwvznb68g35bqc8wkc1gh6416aid7q21ih087630"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("vala" ,vala)
       ("glib:bin" ,glib "bin")
       ("gettext" ,gettext-minimal)
       ("libxml2" ,libxml2)
       ("gobject-introspection"  ,gobject-introspection)))
    (inputs
     `(("pulseaudio" ,pulseaudio)
       ("wingpanel" ,wingpanel)
       ("libcanberra" ,libcanberra)
       ("libnotify" ,libnotify)
       ("libgee" ,libgee)
       ("gtk+" ,gtk+)
       ("granite" ,granite)))
    (home-page "https://github.com/elementary/wingpanel-indicator-sound")
    (synopsis "Wingpanel Sound Indicator")
    (description "Wingpanel Sound Indicator.")
    (license license:gpl3)))

(define-public wingpanel-indicator-network
  (package
    (name "wingpanel-indicator-network")
    (version "2.2.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/elementary/wingpanel-indicator-sound/archive/"
                    version ".tar.gz"))
              (sha256 (base32
                       "0yysaw1v7xs3pyp026nc5lpqkfmdnc9mknl1ad3gcg5khmdpfnn1"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("vala" ,vala)
       ("gettext" ,gettext-minimal)
       ("gobject-introspection" ,gobject-introspection)))
    (inputs
     `(("granite" ,granite)
       ("gtk+" ,gtk+)
       ("libgee" ,libgee)
       ("network-manager" ,network-manager)
       ("network-manager-applet" ,network-manager-applet)
       ("wingpanel" ,wingpanel)))
    (home-page "https://github.com/elementary/wingpanel-indicator-network")
    (synopsis "Wingpanel Network Indicator")
    (description "Wingpanel Network Indicator.")
    (license license:lgpl2.1)))

(define-public wingpanel-indicator-session
  (package
    (name "wingpanel-indicator-session")
    (version "2.2.8")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/elementary/wingpanel-indicator-session/archive/"
                    version ".tar.gz"))
              (sha256 (base32
                       "0sg411k0ql70ny4jmhjdrjj8sz5g080fpsjc67xgynav49sh1jc4"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("vala" ,vala)
       ("gettext" ,gettext-minimal)
       ("gobject-introspection" ,gobject-introspection)))
    (inputs
     `(("accountsservice" ,accountsservice)
       ("granite" ,granite)
       ("gtk+" ,gtk+)
       ("libgee" ,libgee)
       ("wingpanel" ,wingpanel)))
    (home-page "https://github.com/elementary/wingpanel-indicator-session")
    (synopsis "Wingpanel Session Indicator")
    (description "Wingpanel Session Indicator.")
    (license license:gpl2+)))
