;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-

;;; Copyright (C) 2017, Dmitry Ignatiev <lovesan.ru at gmail.com>

;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:

;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(asdf:defsystem #:clave
  :version "0.1.0"
  :description "Common Lisp FFmpeg interface"
  :author "Dmitry Ignatiev <lovesan.ru at gmail.com>"
  :maintainer "Dmitry Ignatiev <lovesan.ru at gmail.com>"
  :licence "MIT"
  :depends-on (#:trivial-garbage #:bordeaux-threads #:cffi #:uiop)
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "lib")
                             (:file "lib-load")
                             (:file "utils")
                             (:file "types")
                             (:file "errors")

                             ;; various enums
                             (:file "codec-id")
                             (:file "pixel-format")
                             (:file "sample-format")
                             (:file "channel-layout")
                             (:file "media-type")
                             (:file "format-flags")
                             (:file "codec-flags")
                             (:file "format-context-flags")
                             (:file "codec-capabilities")
                             (:file "scale-flags")
                             (:file "packet-flags")
                             (:file "frame-flags")
                             (:file "picture-type")
                             (:file "decode-error-flags")
                             (:file "chroma-location")
                             (:file "color-space")
                             (:file "color-range")
                             (:file "color-transfer-characteristic")
                             (:file "color-primaries")
                             (:file "field-order")
                             (:file "disposition")
                             (:file "discard")
                             (:file "stream-parse-type")
                             (:file "io-flags")
                             (:file "compliance")

                             ;; impl
                             (:file "format")
                             (:file "codec")
                             (:file "buffer")
                             (:file "packet")
                             (:file "frame")
                             (:file "codec-parameters")
                             (:file "media-stream")
                             (:file "dict")
                             (:file "io-context")
                             (:file "format-context")
                             (:file "codec-context")
                             ))))

;; vim: ft=lisp et
