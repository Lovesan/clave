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

(cffi:defcstruct (av-codec-parameters :class av-codec-parameters)
  (codec-type media-type)
  (codec-id codec-id)
  (codec-tag :uint32)
  (extradata :pointer)
  (extradata-size :int)
  (format :int)
  (bit-rate :int64)
  (bits-per-coded-sample :int)
  (bits-per-raw-sample :int)
  (profile :int)
  (level :int)
  (width :int)
  (height :int)
  (sample-aspect-ratio (:struct av-rational))
  (field-order field-order)
  (color-range color-range)
  (color-primaries color-primaries)
  (color-trc color-transfer-characteristic)
  (color-space color-space)
  (chroma-location chroma-location)
  (video-delay :int)
  (channel-layout channels-or-layout)
  (channels :int)
  (sample-rate :int)
  (block-align :int)
  (frame-size :int)
  (initial-padding :int)
  (trailing-padding :int)
  (seek-preroll :int))

(defcfun (avcodec-parameters-alloc "avcodec_parameters_alloc" :library libavcodec)
    :pointer)

(defcfun (avcodec-parameters-free "avcodec_parameters_free" :library libavcodec)
    :void
  (pp :pointer))

(defcfun (avcodec-parameters-to-context "avcodec_parameters_to_context" :library libavcodec)
    :int
  (codec :pointer)
  (par :pointer))

(defcfun (avcodec-parameters-from-context "avcodec_parameters_from_context" :library libavcodec)
    :int
  (par :pointer)
  (codec :pointer))

(defcfun (avcodec-parameters-copy "avcodec_parameters_copy" :library libavcodec)
    :int
  (dest :pointer)
  (src :pointer))

(declaim (inline %free-codecpar))
(defun %free-codecpar (p)
  (declare (type foreign-pointer p))
  (with-foreign-object (pp :pointer )
    (setf (mem-ref pp :pointer) p)
    (avcodec-parameters-free pp))
  (values))

(defun parameters-to-context (ctx codecpar)
  (declare (type codec-context ctx)
           (type codec-parameters codecpar))
  (check-rv (avcodec-parameters-to-context
             (%codec-context-ptr ctx)
             (%codec-parameters-ptr codecpar)))
  (values))

(defun parameters-from-context (codecpar ctx)
  (declare (type codec-context ctx)
           (type codec-parameters codecpar))
  (check-rv (avcodec-parameters-from-context             
             (%codec-parameters-ptr codecpar)
             (%codec-context-ptr ctx)))
  (values))

(defmacro %with-codecpar-slots (((&rest vars) codecpar) &body body)
  `(with-foreign-slots ((,@vars)
                        (%codec-parameters-ptr ,codecpar)
                        (:struct av-codec-parameters))
     ,@body))

(defaccessors codec-parameters codecpar- (:struct av-codec-parameters) %codec-parameters-ptr
  (codec-type keyword "Codec type" t)
  (codec-id keyword "Codec id" t)
  (bit-rate int64 "Bit rate" t)
  (bits-per-coded-sample int "Bits per coded sample" t)
  (bits-per-raw-sample int "Bits per raw sample" t)
  ((profile-id profile) int "Codec profile ID" t)
  (level int "Codec profile level" t)
  (width int "Frame width" t)
  (height int "Frame height" t)
  (field-order keyword "Codec field order" t)
  (color-range keyword "Codec color range" t)
  (color-primaries keyword "Codec color primaries" t)
  (color-trc keyword "Codec color transfer characteristics" t)
  (color-space keyword "Codec color space" t)
  (chroma-location keyword "Codec chroma location" t)
  (channels int "Channel count" t)
  (video-delay int "Codec video delay" t)
  (sample-rate int "Codec sample rate" t)
  (block-align int "Codec block alignment" t)
  (frame-size int "Codec audio frame size" t)
  (initial-padding int "Codec initial padding" t)
  (seek-preroll int "Seek preroll" t)
  (sample-aspect-ratio rational "Sample aspect ratio" t)
  (channel-layout (or list keyword) "Codec channel layout" t))

(declaim (inline codecpar-format))
(defun codecpar-format (codecpar)
  "Codec pixel/sample format"
  (declare (type codec-parameters codecpar))
  (%with-codecpar-slots ((format sample-rate) codecpar)
    (if (< format 0)
      nil
      (if (zerop sample-rate)
        (foreign-enum-keyword 'pixel-format format)
        (foreign-enum-keyword 'sample-format format)))))

(declaim (inline (setf codecpar-format)))
(defun (setf codecpar-format) (new-value codecpar)
  "Codec pixel/sample format"
  (declare (type codec-parameters codecpar)
           (type (or null keyword) new-value))
  (%with-codecpar-slots ((format sample-rate) codecpar)
    (let ((new-value (if (null new-value)
                       -1
                       (if (zerop sample-rate)
                         (foreign-enum-value 'pixel-format new-value)
                         (foreign-enum-value 'sample-format new-value)))))
      (setf format new-value)
      new-value)))

(defun make-codec-parameters (&key (codec-id :none)
                                   (codec-type :unknown)
                                   (bit-rate 0)
                                   (bits-per-coded-sample 0)
                                   (bits-per-raw-sample 0)
                                   (profile-id 0)
                                   (level 0)
                                   (width 0)
                                   (height 0)
                                   (field-order :unknown)
                                   (color-range :unspecified)
                                   (color-primaries :unspecified)
                                   (color-trc :unspecified)
                                   (color-space :unspecified)
                                   (chroma-location :unspecified)
                                   (video-delay 0)
                                   (sample-rate 0)
                                   (block-align 0)
                                   (frame-size 0)
                                   (initial-padding 0)
                                   (seek-preroll 0)
                                   (format nil)
                                   (sample-aspect-ratio 0)
                                   (channel-layout nil)
                                   (channels 0))
  (declare (type keyword codec-id codec-type field-order
                 color-range color-primaries color-trc
                 color-space chroma-location)
           (type (or list keyword) channel-layout)
           (type (or null keyword) format)
           (type int64 bit-rate)
           (type rational sample-aspect-ratio)
           (type int32 bits-per-raw-sample bits-per-coded-sample
                 profile-id level width height video-delay
                 sample-rate block-align frame-size
                 initial-padding seek-preroll channels))
  (let ((ptr (avcodec-parameters-alloc)))
    (when (null-pointer-p ptr)
      (error 'out-of-memory))
    (let ((codecpar (%codec-parameters ptr)))
      (finalize codecpar (lambda () (%free-codecpar ptr)))
      (setf (codecpar-codec-id codecpar) codec-id
            (codecpar-codec-type codecpar) codec-type
            (codecpar-bit-rate codecpar) bit-rate
            (codecpar-bits-per-coded-sample codecpar) bits-per-coded-sample
            (codecpar-bits-per-raw-sample codecpar) bits-per-raw-sample
            (codecpar-profile-id codecpar) profile-id
            (codecpar-level codecpar) level
            (codecpar-width codecpar) width
            (codecpar-height codecpar) height
            (codecpar-field-order codecpar) field-order
            (codecpar-color-range codecpar) color-range
            (codecpar-color-primaries codecpar) color-primaries
            (codecpar-color-trc codecpar) color-trc
            (codecpar-color-space codecpar) color-space
            (codecpar-chroma-location codecpar) chroma-location
            (codecpar-video-delay codecpar) video-delay
            (codecpar-sample-rate codecpar) sample-rate
            (codecpar-block-align codecpar) block-align
            (codecpar-frame-size codecpar) frame-size
            (codecpar-initial-padding codecpar) initial-padding
            (codecpar-seek-preroll codecpar) seek-preroll
            (codecpar-sample-aspect-ratio codecpar) sample-aspect-ratio
            (codecpar-format codecpar) format
            (codecpar-channels codecpar) channels
            (codecpar-channel-layout codecpar) channel-layout)
      codecpar)))

(defmethod print-object ((codecpar codec-parameters) stream)
  (if *print-pretty*
    (print-unreadable-object (codecpar stream :type t)
      (pprint-logical-block (stream nil)
        (format stream "Codec id: ~a~:@_" (codecpar-codec-id codecpar))
        (format stream "Codec type: ~a~:@_" (codecpar-codec-type codecpar))
        (let-when (profile (codecpar-profile-id codecpar)
                           (> profile 0))
          (format stream "Profile id: ~a~:@_" profile))
        (let-when (level (codecpar-level codecpar)
                         (> level 0))
          (format stream "Level: ~a~:@_" level))
        (let-when (format (codecpar-format codecpar))
          (format stream "Format: ~a~:@_" format))
        (let-when (bit-rate (codecpar-bit-rate codecpar)
                            (> bit-rate 0))
          (format stream "Bit rate: ~a~:@_" bit-rate))
        (let-when (sample-rate (codecpar-sample-rate codecpar)
                               (> sample-rate 0))
          (format stream "Sample rate: ~a~:@_" sample-rate)
          (format stream "Channels: ~a~:@_" (codecpar-channels codecpar))
          (format stream "Channel layout: ~a~:@_" (codecpar-channel-layout codecpar))
          (format stream "Frame size: ~a~:@_" (codecpar-frame-size codecpar))
          (let-when (bps (codecpar-bits-per-coded-sample codecpar) (> bps 0))
            (format stream "Bits / coded sample ~a~:@_" bps))
          (let-when (bps (codecpar-bits-per-raw-sample codecpar) (> bps 0))
            (format stream "Bits / raw sample ~a~:@_" bps)))
        (let-when (width (codecpar-width codecpar)
                         (> width 0))
          (format stream "Width: ~a~:@_" width)
          (format stream "Height: ~a~:@_" (codecpar-height codecpar))
          (format stream "Video delay: ~a~:@_" (codecpar-video-delay codecpar))
          (let-when (sar (codecpar-sample-aspect-ratio codecpar) (/= sar 0))
            (format stream "Sample aspect ratio: ~a" sar)))
        (format stream "{#x~8,'0X}" (pointer-address (%codec-parameters-ptr codecpar)))))
    (call-next-method)))

;; vim: ft=lisp et
