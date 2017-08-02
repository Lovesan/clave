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

(in-package #:clave)

(defbitfield (codec-capabilities :uint)
  (:draw-horizontal-band #x00000001)
  (:dr1 #x00000002)
  (:truncated #x00000008)
  (:delay #x00000020)
  (:small-last-frame #x00000040)
  (:hwaccel-vdpau #x00000080)
  (:subframes #x00000100)
  (:experimental #x00000200)
  (:channel-configuration #x00000400)
  (:frame-threads #x00001000)
  (:slice-threads #x00002000)
  (:param-change #x00004000)
  (:auto-threads #x00008000)
  (:variable-frame-size #x00010000)
  (:avoid-probing #x00020000)
  (:intra-only #x40000000)
  (:loseless #x80000000))

;; vim: ft=lisp et
