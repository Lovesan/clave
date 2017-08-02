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

(defcstruct av-frame
  (data :pointer :count 8)
  (linesize :int :count 8)
  (extended-data :pointer)
  (width :int)
  (height :int)
  (samples :int)
  (format :int)
  (key-frame :boolean)
  (pict-type picture-type)
  (sar-num :int)
  (sar-denom :int)
  (pts :int64)
  (pkt-pts :int64)
  (pkt-dts :int64)
  (codec-picture-number :int)
  (display-picture-number :int)
  (quality :int)
  (opaque :pointer)
  (errors :uint64 :count 8)
  (repeat-pict :int)
  (interlaced-frame :boolean)
  (top-field-first :boolean)
  (palette-has-changed :boolean)
  (reordered-opaque :int64)
  (sample-rate :int)
  (channel-layout :uint64)
  (buf :pointer :count 8)
  (extended-buf :pointer)
  (nb-extended-bug :int)
  (side-data :pointer)
  (nb-side-data :int)
  (flags frame-flags)
  (color-range color-range)
  (color-primaries color-primaries)
  (col-trc color-transfer-characteristic)
  (colorspace color-space)
  (chrome-location chroma-location)
  (best-effort-timestamp :int64)
  (pkt-pos :int64)
  (pkt-duration :int64)
  (metadata :pointer)
  (decode-error-flags decode-error-flags)
  (channels :int)
  (pkt-size :int)
  (qscale-table :pointer)
  (qstride :int)
  (qscale-type :int)
  (qp-table-buf :pointer)
  (hw-frames-ctx :pointer))

(defmacro with-frame-slots ((var frame accessor-name) &body body)
  `(let ((,var (%frame-ptr ,frame)))
     (declare (ignorable ,var))
     (macrolet ((,accessor-name (slot)
                  `(foreign-slot-value ,',var '(:struct av-frame) ',slot)))
       ,@body)))

(defcfun (av-frame-alloc "av_frame_alloc" :library libavcodec)
    :pointer)

(defcfun (av-frame-free "av_frame_free" :library libavcodec)
    :void
  (pp :pointer))

(declaim (inline %free-frame))
(defun %free-frame (p)
  (declare (type foreign-pointer p))
  (with-foreign-object (pp :pointer)
    (setf (mem-ref pp :pointer) p)
    (av-frame-free pp))
  (values))

(defcfun (av-frame-clone "av_frame_clone" :library libavcodec)
    :pointer
  (p :pointer))

(defcfun (av-frame-is-writable "av_frame_is_writable" :library libavcodec)
    :boolean
  (p :pointer))

(defcfun (av-frame-make-writable "av_frame_make_writable" :library libavcodec)
    :int
  (p :pointer))

(defun make-frame (&optional size)
  (declare (type (or null (signed-byte 32)) size))
  (let ((p (av-frame-alloc)))
    (when (null-pointer-p p)
      (error 'out-of-memory))
    (when size
      (let ((buf (av-buffer-alloc size)))
        (when (null-pointer-p buf)
          (%free-frame p)
          (error 'out-of-memory))
        (setf (mem-ref (foreign-slot-value p '(:struct av-frame) 'buf) :pointer)
              buf
              (mem-ref (foreign-slot-value p '(:struct av-frame) 'data) :pointer)
              (foreign-slot-value buf '(:struct av-buffer-ref) 'data)
              (mem-ref (foreign-slot-value p '(:struct av-frame) 'linesize) :int)
              size
              (foreign-slot-value p '(:struct av-frame) 'extended-data)
              (foreign-slot-value p '(:struct av-frame) 'data))))
    (let ((frame (%frame p)))
      (finalize frame (lambda () (%free-frame p)))
      frame)))

(defun free-frame (frame)
  (declare (type frame frame))
  (let ((ptr (%frame-ptr frame)))
    (unless (null-pointer-p ptr)
      (cancel-finalization frame)
      (%free-frame ptr)
      (setf (%frame-ptr frame) (null-pointer)))
    (values)))

(defmacro with-frame ((var &optional size) &body body)
  `(let ((,var (make-frame ,size)))
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

(defun frame-is-writable (frame)
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
  (with-frame-slots (ptr frame acc)
    (mem-aref (acc data) :pointer index)))

(declaim (inline (setf frame-data)))
(defun (setf frame-data) (new-value frame index)
  (declare (type frame frame)
           (type (integer 0 7) index)
           (type foreign-pointer new-value))
  (with-frame-slots (ptr frame acc)
    (setf (mem-aref (acc data) :pointer index) new-value)))

(declaim (inline frame-data-address))
(defun frame-data-address (frame)
  (declare (type frame frame))
  (with-frame-slots (ptr frame acc)
    (acc data)))

(declaim (inline frame-linesize))
(defun frame-linesize (frame index)
  (declare (type frame frame)
           (type (integer 0 7) index))
  (with-frame-slots (ptr frame acc)
    (mem-aref (acc linesize) :int index)))

(declaim (inline (setf frame-linesize)))
(defun (setf frame-linesize) (new-value frame index)
  (declare (type frame frame)
           (type (integer 0 7) index)
           (type int32 new-value))
  (with-frame-slots (ptr frame acc)
    (setf (mem-aref (acc linesize) :int index) new-value)))

(declaim (inline frame-linesize-address))
(defun frame-linesize-address (frame)
  (declare (type frame frame))
  (with-frame-slots (ptr frame acc)
    (acc linesize)))

(declaim (inline frame-extended-data))
(defun frame-extended-data (frame)
  (declare (type frame frame))
  (with-frame-slots (ptr frame acc)
    (acc extended-data)))

(declaim (inline frame-extended-data))
(defun (setf frame-extended-data) (new-value frame)
  (declare (type frame frame)
           (type foreign-pointer new-value))
  (with-frame-slots (ptr frame acc)
    (setf (acc extended-data) new-value)))

(declaim (inline frame-width))
(defun frame-width (frame)
  (declare (type frame frame))
  (with-frame-slots (ptr frame acc)
    (acc width)))

(declaim (inline (setf frame-width)))
(defun (setf frame-width) (new-value frame)
  (declare (type frame frame)
           (type int32 new-value))
  (with-frame-slots (ptr frame acc)
    (setf (acc width) new-value)))

(declaim (inline frame-height))
(defun frame-height (frame)
  (declare (type frame frame))
  (with-frame-slots (ptr frame acc)
    (acc height)))

(declaim (inline (setf frame-height)))
(defun (setf frame-height) (new-value frame)
  (declare (type frame frame)
           (type int32 new-value))
  (with-frame-slots (ptr frame acc)
    (setf (acc height) new-value)))

(declaim (inline frame-samples))
(defun frame-samples (frame)
  (declare (type frame frame))
  (with-frame-slots (ptr frame acc)
    (acc samples)))

(declaim (inline (setf frame-samples)))
(defun (setf frame-samples) (new-value frame)
  (declare (type frame frame)
           (type int32 new-value))
  (with-frame-slots (ptr frame acc)
    (setf (acc samples) new-value)))

(declaim (inline frame-format))
(defun frame-format (frame)
  (declare (type frame frame))
  (with-frame-slots (ptr frame acc)
    (let ((fmt (acc format)))
      (if (< fmt 0)
        nil
        (if (zerop (acc sample-rate))
          (foreign-enum-keyword 'pixel-format fmt)
          (foreign-enum-keyword 'sample-format fmt))))))

(declaim (inline (setf frame-format)))
(defun (setf frame-format) (new-value frame)
  (declare (type frame frame)
           (type keyword new-value))
  (with-frame-slots (ptr frame acc)
    (let ((new-value (if (null new-value)
                       -1
                       (if (zerop (acc sample-rate))
                         (foreign-enum-value 'pixel-format new-value)
                         (foreign-enum-value 'sample-format new-value)))))
      (setf (acc format) new-value))
    new-value))

(declaim (inline frame-sample-rate))
(defun frame-sample-rate (frame)
  (declare (type frame frame))
  (with-frame-slots (ptr frame acc)
    (acc sample-rate)))

(declaim (inline (setf frame-sample-rate)))
(defun (setf frame-sample-rate) (new-value frame)
  (declare (type frame frame)
           (type int32 new-value))
  (with-frame-slots (ptr frame acc)
    (setf (acc sample-rate) new-value)))

(declaim (inline frame-key-p))
(defun frame-key-p (frame)
  (declare (type frame frame))
  (with-frame-slots (ptr frame acc)
    (acc key-frame)))

(declaim (inline (setf frame-key-p)))
(defun (setf frame-key-p) (new-value frame)
  (declare (type frame frame)
           (type boolean new-value))
  (with-frame-slots (ptr frame acc)
    (setf (acc key-frame) new-value)))

(declaim (inline frame-picture-type))
(defun frame-picture-type (frame)
  (declare (type frame frame))
  (with-frame-slots (ptr frame acc)
    (acc pict-type)))

(declaim (inline (setf frame-picture-type)))
(defun (setf frame-picture-type) (new-value frame)
  (declare (type frame frame)
           (type keyword new-value))
  (with-frame-slots (ptr frame acc)
    (setf (acc pict-type) new-value)))

(declaim (inline frame-sample-aspect-ratio))
(defun frame-sample-aspect-ratio (frame)
  (declare (type frame frame))
  (with-frame-slots (ptr frame acc)
    (let ((n (acc sar-num))
          (d (acc sar-denom)))
      (if (zerop d)
        0
        (/ n d)))))

(declaim (inline (setf frame-sample-aspect-ratio)))
(defun (setf frame-sample-aspect-ratio) (new-value frame)
  (declare (type frame frame)
           (type rational new-value))
  (with-frame-slots (ptr frame acc)
    (setf (acc sar-num) (numerator new-value)
          (acc sar-denom) (denominator new-value))
    new-value))

(declaim (inline frame-pts))
(defun frame-pts (frame)
  (declare (type frame frame))
  (with-frame-slots (ptr frame acc)
    (acc pts)))

(declaim (inline (setf frame-pts)))
(defun (setf frame-pts) (new-value frame)
  (declare (type frame frame)
           (type int64 new-value))
  (with-frame-slots (ptr frame acc)
    (setf (acc pts) new-value)))

(declaim (inline frame-packet-dts))
(defun frame-packet-dts (frame)
  (declare (type frame frame))
  (with-frame-slots (ptr frame acc)
    (acc pkt-dts)))

(declaim (inline (setf frame-packet-dts)))
(defun (setf frame-packet-dts) (new-value frame)
  (declare (type frame frame)
           (type int64 new-value))
  (with-frame-slots (ptr frame acc)
    (setf (acc pkt-dts) new-value)))

(declaim (inline frame-coded-picture-number))
(defun frame-coded-picture-number (frame)
  (declare (type frame frame))
  (with-frame-slots (ptr frame acc)
    (acc codec-picture-number)))

(declaim (inline frame-display-picture-number))
(defun frame-display-picture-number (frame)
  (declare (type frame frame))
  (with-frame-slots (ptr frame acc)
    (acc display-picture-number)))

(declaim (inline frame-quality))
(defun frame-quality (frame)
  (declare (type frame frame))
  (with-frame-slots (ptr frame acc)
    (acc quality)))

(declaim (inline frame-opaque))
(defun frame-opaque (frame)
  (declare (type frame frame))
  (with-frame-slots (ptr frame acc)
    (acc opaque)))

(declaim (inline (setf frame-opaque)))
(defun (setf frame-opaque) (new-value frame)
  (declare (type frame frame)
           (type foreign-pointer new-value))
  (with-frame-slots (ptr frame acc)
    (setf (acc opaque) new-value)))

(declaim (inline frame-repeat-picture))
(defun frame-repeat-picture (frame)
  (declare (type frame frame))
  (with-frame-slots (ptr frame acc)
    (acc repeat-pict)))

(declaim (inline frame-interlaced-p))
(defun frame-interlaced-p (frame)
  (declare (type frame frame))
  (with-frame-slots (ptr frame acc)
    (acc interlaced-frame)))

(declaim (inline frame-top-field-first-p))
(defun frame-top-field-first-p (frame)
  (declare (type frame frame))
  (with-frame-slots (ptr frame acc)
    (acc top-field-first)))

(declaim (inline frame-palette-has-changed))
(defun frame-palette-has-changed (frame)
  (declare (type frame frame))
  (with-frame-slots (ptr frame acc)
    (acc palette-has-changed)))

(declaim (inline frame-reordered-opaque))
(defun frame-reordered-opaque (frame)
  (declare (type frame frame))
  (with-frame-slots (ptr frame acc)
    (acc reordered-opaque)))

(declaim (inline frame-channel-layout))
(defun frame-channel-layout (frame)
  (declare (type frame frame))
  (with-frame-slots (ptr frame acc)
    (to-channel-layout (acc channel-layout))))

(declaim (inline (setf frame-channel-layout)))
(defun (setf frame-channel-layout) (new-value frame)
  (declare (type frame frame)
           (type (or list keyword) new-value))
  (with-frame-slots (ptr frame acc)
    (setf (acc channel-layout) (from-channel-layout new-value))))

(declaim (inline frame-flags))
(defun frame-flags (frame)
  (declare (type frame frame))
  (with-frame-slots (ptr frame acc)
    (acc flags)))

(declaim (inline frame-color-range))
(defun frame-color-range (frame)
  (declare (type frame frame))
  (with-frame-slots (ptr frame acc)
    (acc color-range)))

(declaim (inline frame-color-primaries))
(defun frame-color-primaries (frame)
  (declare (type frame frame))
  (with-frame-slots (ptr frame acc)
    (acc color-primaries)))

(declaim (inline frame-color-trc))
(defun frame-color-trc (frame)
  (declare (type frame frame))
  (with-frame-slots (ptr frame acc)
    (acc col-trc)))

(declaim (inline frame-colorspace))
(defun frame-colorspace (frame)
  (declare (type frame frame))
  (with-frame-slots (ptr frame acc)
    (acc colorspace)))

(declaim (inline frame-chroma-location))
(defun frame-chroma-location (frame)
  (declare (type frame frame))
  (with-frame-slots (ptr frame acc)
    (acc chrome-location)))

(declaim (inline frame-best-effort-timestamp))
(defun frame-best-effort-timestamp (frame)
  (declare (type frame frame))
  (with-frame-slots (ptr frame acc)
    (acc best-effort-timestamp)))

(declaim (inline frame-packet-position))
(defun frame-packet-position (frame)
  (declare (type frame frame))
  (with-frame-slots (ptr frame acc)
    (acc pkt-pos)))

(declaim (inline frame-packet-duration))
(defun frame-packet-duration (frame)
  (declare (type frame frame))
  (with-frame-slots (ptr frame acc)
    (acc pkt-duration)))

(declaim (inline frame-decode-error-flags))
(defun frame-decode-error-flags (frame)
  (declare (type frame frame))
  (with-frame-slots (ptr frame acc)
    (acc decode-error-flags)))

(declaim (inline frame-channels))
(defun frame-channels (frame)
  (declare (type frame frame))
  (with-frame-slots (ptr frame acc)
    (acc channels)))

(declaim (inline (setf frame-channels)))
(defun (setf frame-channels) (new-value frame)
  (declare (type frame frame)
           (type int32 new-value))
  (with-frame-slots (ptr frame acc)
    (setf (acc channels) new-value)))

(declaim (inline frame-packet-size))
(defun frame-packet-size (frame)
  (declare (type frame frame))
  (with-frame-slots (ptr frame acc)
    (acc pkt-size)))

(declaim (inline (setf frame-packet-size)))
(defun (setf frame-packet-size) (new-value frame)
  (declare (type frame frame)
           (type int32 new-value))
  (with-frame-slots (ptr frame acc)
    (setf (acc pkt-size) new-value)))

;; vim: ft=lisp et
