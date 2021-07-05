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

(defconstant +frame-data-pointers+ 8)

(defcstruct (av-frame :class av-frame)
  (data :pointer :count #.+frame-data-pointers+)
  (linesize :int :count #.+frame-data-pointers+)
  (extended-data :pointer)
  (width :int)
  (height :int)
  (samples :int)
  (format :int)
  (key-frame :boolean)
  (pict-type picture-type)
  (sample-aspect-ratio (:struct av-rational))
  (pts :int64)
  (pkt-pts :int64) ;; deprecated
  (pkt-dts :int64)
  (coded-picture-number :int)
  (display-picture-number :int)
  (quality :int)
  (opaque :pointer)
  (errors :uint64 :count #.+frame-data-pointers+) ;; deprecated
  (repeat-pict :int)
  (interlaced-frame :boolean)
  (top-field-first :boolean)
  (palette-has-changed :boolean)
  (reordered-opaque :int64)
  (sample-rate :int)
  (channel-layout channels-or-layout)
  (buf :pointer :count #.+frame-data-pointers+)
  (extended-buf :pointer)
  (nb-extended-buf :int)
  (side-data :pointer)
  (nb-side-data :int)
  (flags frame-flags)
  (color-range color-range)
  (color-primaries color-primaries)
  (color-trc color-transfer-characteristic)
  (color-space color-space)
  (chroma-location chroma-location)
  (best-effort-timestamp :int64)
  (pkt-pos :int64)
  (pkt-duration :int64)
  (metadata :pointer)
  (decode-error-flags decode-error-flags)
  (channels :int)
  (pkt-size :int)
  (qscale-table :pointer) ;; deprecated
  (qstride :int) ;; deprecated
  (qscale-type :int) ;; deprecated
  (qp-table-buf :pointer) ;; newly deprecated
  (hw-frames-ctx :pointer)
  (opaque-ref :pointer)
  (crop-top size-t)
  (crop-bottom size-t)
  (crop-left size-t)
  (crop-right size-t)
  (private-ref :pointer)
  )

(defcfun (av-frame-alloc "av_frame_alloc" :library libavcodec)
    :pointer)

(defcfun (av-frame-free "av_frame_free" :library libavcodec)
    :void
  (pp :pointer))

(defcfun (av-frame-clone "av_frame_clone" :library libavcodec)
    :pointer
  (p :pointer))

(defcfun (av-frame-is-writable "av_frame_is_writable" :library libavcodec)
    :boolean
  (p :pointer))

(defcfun (av-frame-make-writable "av_frame_make_writable" :library libavcodec)
    :int
  (p :pointer))

(declaim (inline %free-frame))
(defun %free-frame (p)
  (declare (type foreign-pointer p))
  (with-foreign-object (pp :pointer)
    (setf (mem-ref pp :pointer) p)
    (av-frame-free pp))
  (values))

(defun free-frame (frame)
  (declare (type frame frame))
  (let ((ptr (%frame-ptr frame)))
    (unless (null-pointer-p ptr)
      (cancel-finalization frame)
      (%free-frame ptr)
      (setf (%frame-ptr frame) (null-pointer)))
    (values)))

(defaccessors frame frame- (:struct av-frame) %frame-ptr
  ((data-pointer data) foreign-pointer "Frame data array pointer")
  ((linesize-pointer linesize) foreign-pointer "Frame linesize array pointer")
  (extended-data foreign-pointer "Frame extended data array pointer" t)
  (width int "Frame width" t)
  (height int "Frame height" t)
  (samples int "Frame sample count" t)
  (sample-rate int "Frame sample rate" t)
  ((key-p key-frame) T "Is frame key frame" t)
  ((picture-type pict-type) keyword "Frame picture type" t)
  (sample-aspect-ratio rational "Frame sample aspect ratio" t)
  (pts int64 "Frame PTS" t)
  ((packet-dts pkt-dts) int64 "Frame packet DTS" t)
  (coded-picture-number int "Coded picture number" t)
  (display-picture-number int "Display picture number" t)
  (quality int "Frame quality" t)
  (opaque foreign-pointer "Frame opaque data" t)
  ((repeat-picture repeat-pict) int "Frame repeat picture count" t)
  ((interlaced-p interlaced-frame) t "Is frame interlaced" t)
  ((top-field-first-p top-field-first) t "Is top field first" t)
  ((palette-changed-p palette-has-changed) t "Has palette changed" t)
  (reordered-opaque int64 "Frame reordered opaque" t)
  (channel-layout (or keyword list) "Frame channel layout" t)
  (flags (or keyword list) "Frame flags" t)
  (color-range keyword "Color range" t)
  (color-primaries keyword "Color primaries" t)
  (color-trc keyword "Color transfer characteristics" t)
  (color-space keyword "Color space" t)
  (chroma-location keyword "Chroma location" t)
  (best-effort-timestamp int64 "Frame best effort timestamp" t)
  ((packet-position pkt-pos) int64 "Frame packet position" t)
  ((packet-duration pkt-duration) int64 "Frame packet duration" t)
  (decode-error-flags (or list keyword) "Frame decode error flags" t)
  (channels int "Frame channel count" t)
  ((packet-size pkt-size) int "Frame packet size" t))


(defun make-frame (&key size
                        (pts +no-pts+)
                        (packet-dts +no-pts+)
                        (packet-position -1)
                        (packet-duration 0)
                        (packet-size -1)
                        (format nil)
                        (width 0)
                        (height 0)
                        (samples 0)
                        (sample-rate 0)
                        (channels 0)
                        channel-layout
                        (key-p t)
                        (picture-type :I)
                        (sample-aspect-ratio 0)
                        (opaque (null-pointer))
                        flags
                        (color-primaries :unspecified)
                        (color-trc :unspecified)
                        (color-space :unspecified)
                        (color-range :unspecified)
                        (chroma-location :unspecified)
                        (quality 0))
  (declare (type (or null (signed-byte 32)) size)
           (type int64 pts packet-dts packet-position
                 packet-duration)
           (type foreign-pointer opaque)
           (type int packet-size width height samples sample-rate
                 channels quality)
           (type (or keyword list) format channel-layout flags)
           (type rational sample-aspect-ratio)
           (type keyword color-primaries picture-type color-trc
                 color-range color-space chroma-location))
  (let ((p (av-frame-alloc)))
    (when (null-pointer-p p)
      (error 'out-of-memory))
    (with-foreign-slots ((buf data extended-data linesize)
                         p
                         (:struct av-frame))
      (setf extended-data data)
      (when size
        (let ((buffer-ref (av-buffer-alloc size)))
          (when (null-pointer-p buffer-ref)
            (%free-frame p)
            (error 'out-of-memory))
          (setf (mem-ref buf :pointer)
                buffer-ref
                (mem-ref data :pointer)
                (foreign-slot-value buf '(:struct av-buffer-ref) 'data)
                (mem-ref linesize :int)
                size))))
    (let ((frame (%frame p)))
      (finalize frame (lambda () (%free-frame p)))
      (setf (frame-pts frame) pts
            (frame-packet-dts frame) packet-dts
            (frame-packet-position frame) packet-position
            (frame-packet-duration frame) packet-duration
            (frame-packet-size frame) packet-size
            (frame-key-p frame) key-p
            (frame-opaque frame) opaque
            (frame-flags frame) flags
            (frame-quality frame) quality
            (frame-width frame) width
            (frame-height frame) height
            (frame-samples frame) samples
            (frame-sample-rate frame) sample-rate
            (frame-channels frame) channels
            (frame-channel-layout frame) channel-layout
            (frame-picture-type frame) picture-type
            (frame-sample-aspect-ratio frame) sample-aspect-ratio
            (frame-color-primaries frame) color-primaries
            (frame-color-trc frame) color-trc
            (frame-color-space frame) color-space
            (frame-color-range frame) color-range
            (frame-chroma-location frame) chroma-location
            (frame-format frame) format)
      frame)))

(declaim (inline frame-format))
(defun frame-format (frame)
  (declare (type frame frame))
  (let ((fmt (foreign-slot-value (%frame-ptr frame)
                                 '(:struct av-frame)
                                 'format)))
    (if (< fmt 0)
      nil
      (if (zerop (frame-sample-rate frame))
        (foreign-enum-keyword 'pixel-format fmt)
        (foreign-enum-keyword 'sample-format fmt)))))

(declaim (inline (setf frame-format)))
(defun (setf frame-format) (new-value frame)
  (declare (type frame frame)
           (type (or keyword null) new-value))
  (let ((new-value (if (null new-value)
                     -1
                     (if (zerop (frame-sample-rate frame))
                       (foreign-enum-value 'pixel-format new-value)
                       (foreign-enum-value 'sample-format new-value)))))
    (setf (foreign-slot-value (%frame-ptr frame)
                              '(:struct av-frame)
                              'format)
          new-value))
  new-value)

(declaim (inline frame-alive-p))
(defun frame-alive-p (frame)
  (declare (type frame frame))
  (not (null-pointer-p (%frame-ptr frame))))

(defmacro with-frame ((var &rest args) &body body)
  `(let ((,var (make-frame ,@args)))
     (declare (dynamic-extent ,var))
     (unwind-protect (locally ,@body)
       (free-frame ,var))))

(defun clone-frame (frame)
  (declare (type frame frame))
  (let ((p (av-frame-clone (%frame-ptr frame))))
    (when (null-pointer-p p)
      (error 'out-of-memory))
    (let ((frame (%frame p)))
      (finalize frame (lambda () (%free-frame p)))
      frame)))

(defun frame-writable-p (frame)
  (declare (type frame frame))
  (av-frame-is-writable (%frame-ptr frame)))

(defun frame-make-writable (frame)
  (declare (type frame frame))
  (check-rv (av-frame-make-writable (%frame-ptr frame)))
  (values))

(declaim (inline frame-data))
(defun frame-data (frame index)
  (declare (type frame frame)
           (type (integer 0 7) index))
  (mem-aref (foreign-slot-pointer (%frame-ptr frame)
                                  '(:struct av-frame)
                                  'data)
            :pointer
            index))

(declaim (inline (setf frame-data)))
(defun (setf frame-data) (new-value frame index)
  (declare (type frame frame)
           (type (integer 0 7) index)
           (type foreign-pointer new-value))
  (setf (mem-aref (foreign-slot-pointer (%frame-ptr frame)
                                        '(:struct av-frame)
                                        'data)
                  :pointer
                  index)
        new-value))

(declaim (inline frame-linesize))
(defun frame-linesize (frame index)
  (declare (type frame frame)
           (type (integer 0 7) index))
  (mem-aref (foreign-slot-pointer (%frame-ptr frame)
                                  '(:struct av-frame)
                                  'linesize)
            :int
            index))

(declaim (inline (setf frame-linesize)))
(defun (setf frame-linesize) (new-value frame index)
  (declare (type frame frame)
           (type (integer 0 7) index)
           (type int32 new-value))
  (setf (mem-aref (foreign-slot-pointer (%frame-ptr frame)
                                        '(:struct av-frame)
                                        'linesize)
                  :int
                  index)
        new-value))

(defmethod print-object ((frame frame) stream)
  (if *print-pretty*
    (print-unreadable-object (frame stream :type t :identity t)
      (if (frame-alive-p frame)
        (pprint-logical-block (stream nil)
          (write-string "Data: [" stream)
          (dotimes (i (1- +frame-data-pointers+))
            (format stream "#x~8,'0X, ~:_" (pointer-address (frame-data frame i))))
          (format stream "#x~8,'0X]~:@_" (pointer-address (frame-data frame 7)))
          (write-string "Linesize: [" stream)
          (dotimes (i (1- +frame-data-pointers+))
            (format stream "~a, ~:_" (frame-linesize frame i)))
          (format stream "~a]~:@_" (frame-linesize frame 7))
          (let-when (pts (frame-pts frame) (/= pts +no-pts+))
            (format stream "PTS: ~a~:@_" pts))
          (let-when (dts (frame-packet-dts frame) (/= dts +no-pts+))
            (format stream "Packet DTS: ~a~:@_" dts))
          (let-when (bet (frame-best-effort-timestamp frame) (/= bet +no-pts+))
            (format stream "Best effort timestamp: ~a~:@_" bet))
          (let-when (flags (frame-flags frame))
            (format stream "Flags: ~{~a~^, ~:_~}~:@_" flags))
          (let-when (quality (frame-quality frame)
                             (not (zerop quality)))
            (format stream "Quality: ~a~:@_" quality))
          (let-when (format (frame-format frame))
            (format stream "Format: ~a~:@_" format))
          (let-when (sample-rate (frame-sample-rate frame) (> sample-rate 0))
            (format stream "Sample rate: ~a~:@_" sample-rate)
            (format stream "Sample count: ~a~:@_" (frame-samples frame))
            (format stream "Channels: ~a~:@_" (frame-channels frame))
            (format stream "Channel layout: ~a~:@_" (frame-channel-layout frame)))
          (let-when (width (frame-width frame) (> width 0))
            (format stream "Width: ~a~:@_" width)
            (format stream "Height: ~a~:@_" (frame-height frame))
            (format stream "Picture type: ~a~:@_" (frame-picture-type frame))
            (format stream "Is key frame: ~a~:@_" (frame-key-p frame))
            (let-when (sar (frame-sample-aspect-ratio frame)
                           (not (zerop sar)))
              (format stream "Sample aspect ratio: ~a~:@_" sar))
            (let-when (color-range (frame-color-range frame)
                                   (not (eq color-range :unspecified)))
              (format stream "Color range: ~a~:@_" color-range))))
        (format stream "Disposed")))
    (call-next-method)))

;; vim: ft=lisp et
