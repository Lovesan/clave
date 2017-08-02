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

(in-package :cl-user)

(uiop:define-package #:clave
  (:use #:cl #:cffi #:trivial-garbage)
  (:export

   ;; enums
   
   #:codec-id
   #:pixel-format
   #:sample-format
   #:channels
   #:channel-layout
   #:media-type
   #:format-flags
   #:codec-flags
   #:format-context-flags
   #:codec-capabilities
   #:scale-flags
   #:package-flags
   #:frame-flags
   #:color-range
   #:color-space
   #:color-transfer-characteristic
   #:color-primaries
   #:chroma-location
   #:decode-error-flags

   ;; utils
   
   #:get-channel-layout
   #:get-channel-layout-string
   #:get-channel-layout-channels
   #:get-default-channel-layout

   #:+no-pts+

   ;; format

   #:io-format
   #:io-format-p
   #:io-format-input-p
   #:find-input-format
   #:guess-output-format
   #:io-format-name
   #:io-format-long-name
   #:io-format-mime-type
   #:io-format-flags
   #:io-format-extensions
   #:io-format-audio-codec
   #:io-format-video-codec
   #:io-format-subtitle-codec

   ;; codec

   #:codec
   #:codecp
   #:codec-name
   #:codec-long-name
   #:codec-type
   #:codec-id
   #:codec-capabilities
   #:codec-framerates
   #:codec-pixel-formats
   #:codec-samplerates
   #:codec-sample-formats
   #:codec-channel-layouts
   #:codec-max-lowres
   #:codec-profiles
   #:codec-profile
   #:codec-profile-p
   #:codec-profile-name
   #:codec-profile-id
   #:find-decoder
   #:find-encoder
   #:find-decoder-by-name
   #:find-encoder-by-name

   ;; packet

   #:packet
   #:packetp
   #:make-packet
   #:clone-packet
   #:packet-alive-p
   #:free-packet
   #:with-packet
   #:shrink-packet
   #:grow-packet
   #:packet-rescale-ts
   #:packet-data
   #:packet-pts
   #:packet-dts
   #:packet-size
   #:packet-flags
   #:packet-duration
   #:packet-position
   #:packet-stream-index

   ;; frame

   #:frame
   #:framep
   #:make-frame
   #:free-frame
   #:with-frame
   #:clone-frame
   #:frame-is-writable
   #:frame-make-writable
   #:frame-data
   #:frame-data-address
   #:frame-linesize
   #:frame-linesize-address
   #:frame-extended-data
   ))

;; vim: ft=lisp et
