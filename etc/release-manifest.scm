;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Ludovic Courtès <ludo@gnu.org>
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

;;; This file returns a manifest containing release-critical bit, for all the
;;; supported architectures and cross-compilation targets.

(use-modules (gnu packages)
             (guix packages)
             (guix profiles)
             ((gnu ci) #:select (%cross-targets))
             (srfi srfi-1)
             (srfi srfi-26))

(define* (package->manifest-entry* package system
                                   #:key target)
  "Return a manifest entry for PACKAGE on SYSTEM, optionally cross-compiled to
TARGET."
  (manifest-entry
    (inherit (package->manifest-entry package))
    (name (string-append (package-name package) "." system
                         (if target
                             (string-append "." target)
                             "'")))
    (item (with-parameters ((%current-system system)
                            (%current-target-system target))
            package))))

(define %base-packages
  ;; Packages that must be substitutable on all the platforms Guix supports.
  (map specification->package
       '("bootstrap-tarballs" "gcc-toolchain" "nss-certs"
         "openssh" "emacs" "vim" "python" "guile" "guix")))

(define %system-packages
  ;; Key packages proposed by the Guix System installer.
  (map specification->package
       '("xorg-server" "xfce" "gnome" "mate" "enlightenment"
         "openbox" "awesome" "i3-wm" "ratpoison"
         "xlockmore" "slock" "libreoffice"
         "connman" "network-manager" "network-manager-applet"
         "openssh" "ntp" "tor"
         "linux-libre" "grub-hybrid"
         ;; FIXME: Add IceCat when Rust is available on i686.
         ;;"icecat"
         )))

(define %packages-to-cross-build
  ;; Packages that must be cross-buildable from x86_64-linux.
  (cons (@ (gnu packages gcc) gcc)
        (map specification->package
             '("coreutils" "grep" "sed" "findutils" "diffutils" "patch"
               "gawk" "gettext" "gzip" "xz"
               "hello" "guile@2.2" "zlib"))))

(define %cross-bootstrap-targets
  ;; Cross-compilation triplets for which 'bootstrap-tarballs' must be
  ;; buildable.
  '("i586-pc-gnu"
    "arm-linux-gnueabihf"
    "aarch64-linux-gnu"))


;;;
;;; Manifests.
;;;

(define %base-manifest
  (manifest
   (append-map (lambda (system)
                 (map (cut package->manifest-entry* <> system)
                      %base-packages))
               %hydra-supported-systems)))

(define %cross-manifest
  (manifest
   (append-map (lambda (target)
                 (map (cut package->manifest-entry* <> "x86_64-linux"
                           #:target target)
                      %packages-to-cross-build))
               %cross-targets)))

(define %cross-bootstrap-manifest
  (manifest
   (map (lambda (target)
          (package->manifest-entry*
           (specification->package "bootstrap-tarballs")
           "x86_64-linux" #:target target))
        %cross-bootstrap-targets)))

;; Return the union of all three manifests.
(concatenate-manifests (list %base-manifest
                             %cross-manifest
                             %cross-bootstrap-manifest))