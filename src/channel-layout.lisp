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

(defbitfield (channels :ulong)
  (:front-left #x1)
  (:front-right #x2)
  (:font-center #x4)
  (:low-frequency #x8)
  (:back-left #x10)
  (:back-right #x20)
  (:front-left-of-center #x40)
  (:front-right-of-center #x80)
  (:back-center #x100)
  (:side-left #x200)
  (:side-right #x400)
  (:top-center #x800)
  (:top-front-left #x1000)
  (:top-front-center #x2000)
  (:top-front-right #x4000)
  (:top-back-left #x8000)
  (:top-back-center #x10000)
  (:top-back-right #x20000)
  (:stereo-left #x20000000)
  (:stereo-right #x40000000)
  (:wide-left   #x80000000)
  (:wide-right #x100000000)
  (:surround-direct-left #x200000000)
  (:surround-direct-right #x400000000)
  (:low-frequency-2 #x800000000))

(defcenum (channel-layout :ulong)
  (:native #x8000000000000000)
  (:mono #x0000000000000004)
  (:stereo #x0000000000000003)
  (:2-point-1 #x000000000000000B)
  (:2-1 #x0000000000000103)
  (:surround #x0000000000000007)
  (:3-point-1 #x000000000000000F)
  (:4-point-0 #x0000000000000107)
  (:4-point-1 #x000000000000010F)
  (:2-2 #x0000000000000603)
  (:quad #x0000000000000033)
  (:5-point-0 #x0000000000000607)
  (:5-point-1 #x000000000000060F)
  (:5-point-0-back #x0000000000000037)
  (:5-point-1-back #x000000000000003F)
  (:6-point-0 #x0000000000000707)
  (:6-point-0-front #x00000000000006c3)
  (:hexagonal #x0000000000000137)
  (:6-point-1 #x000000000000070f)
  (:6-point-1-back #x000000000000013F)
  (:6-point-1-front #x00000000000006cb)
  (:7-point-0 #x0000000000000637)
  (:7-point-0-front #x00000000000006c7)
  (:7-point-1 #x000000000000063f)
  (:7-point-1-wide #x00000000000006cf)
  (:7-point-1-wide-back #x00000000000000ff)
  (:octagonal #x0000000000000737)
  (:hexadecagonal #x18003f737)
  (:stereo-downmix #x60000000))

(declaim (inline to-channel-layout))
(defun to-channel-layout (x)
  (declare (type (unsigned-byte 64) x))
  (or (foreign-enum-keyword 'channel-layout x :errorp nil)
      (foreign-bitfield-symbols 'channels x)))

(declaim (inline from-channel-layout))
(defun from-channel-layout (x)
  (declare (type (or keyword list) x))
  (if (listp x)
    (foreign-bitfield-value 'channels x)
    (or (foreign-enum-value 'channel-layout x :errorp nil)
        0)))

(defcfun (av-get-channel-layout "av_get_channel_layout" :library libavutil)
    :uint64
  (name :pointer))

(defun get-channel-layout (name)
  (declare (type string name))
  (with-foreign-pointer (p 128)
    (lisp-string-to-foreign name p 128 :encoding :utf-8)
    (to-channel-layout (av-get-channel-layout p))))

(defcfun (av-get-channel-layout-string "av_get_channel_layout_string" :library libavutil)
    :void
  (buf :pointer)
  (size :int)
  (channels :int)
  (layout :uint64))

(defun get-channel-layout-string (channels layout)
  (declare (type (signed-byte 32) channels)
           (type (or keyword list) layout))
  (with-foreign-pointer (p 128)
    (av-get-channel-layout-string p 128 channels (from-channel-layout layout))
    (values (foreign-string-to-lisp p :encoding :utf-8))))

(defcfun (av-get-channel-layout-channels "av_get_channel_layout_nb_channels" :library libavutil)
    :int
  (layout :uint64))

(defun get-channel-layout-channels (layout)
  (declare (type (or keyword list) layout))
  (av-get-channel-layout-channels (from-channel-layout layout)))

(defcfun (av-get-default-channel-layout "av_get_default_channel_layout" :library libavutil)
    :uint64
  (channels :int))

(defun get-default-channel-layout (channels)
  (declare (type (signed-byte 32) channels))
  (to-channel-layout (av-get-default-channel-layout channels)))

;; vim: ft=lisp et
