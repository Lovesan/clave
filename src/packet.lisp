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

(defcstruct av-packet
  (buf :pointer)
  (pts :int64)
  (dts :int64)
  (data :pointer)
  (size :int)
  (stream-index :int)
  (flags packet-flags)
  (side-data :pointer)
  (side-data-elems :pointer)
  (duration :int64)
  (pos :int64)
  (convergence-duration :int64))

(defmacro with-packet-slots ((var packet accessor-name) &body body)
  `(let ((,var (%packet-ptr ,packet)))
     (declare (ignorable ,var))
     (macrolet ((,accessor-name (slot)
                  `(foreign-slot-value ,',var '(:struct av-packet) ',slot)))
       ,@body)))

(declaim (inline av-packet-alloc))
(defcfun (av-packet-alloc "av_packet_alloc" :library libavcodec)
    :pointer)

(declaim (inline av-packet-free))
(defcfun (av-packet-free "av_packet_free" :library libavcodec)
    :void
  (pp :pointer))

(declaim (inline av-new-packet))
(defcfun (av-new-packet "av_new_packet" :library libavcodec)
    :int
  (p :pointer)
  (size :int))

(declaim (inline av-shrink-packet))
(defcfun (av-shrink-packet "av_shrink_packet" :library libavcodec)
    :void
  (p :pointer)
  (size :int))

(declaim (inline av-grow-packet))
(defcfun (av-grow-packet "av_grow_packet" :library libavcodec)
    :int
  (p :pointer)
  (by :int))

(declaim (inline av-packet-clone))
(defcfun (av-packet-clone "av_packet_clone" :library libavcodec)
    :pointer
  (p :pointer))

(declaim (inline av-packet-rescale-ts))
(defcfun (av-packet-rescale-ts "av_packet_rescale_ts" :library libavcodec)
    :void
  (p :pointer)
  (src :uint64)
  (dst :uint64))

(declaim (inline %free-packet))
(defun %free-packet (p)
  (declare (type foreign-pointer p))
  (with-foreign-object (pp :pointer)
    (setf (mem-ref pp :pointer) p)
    (av-packet-free pp))
  (values))

(defun make-packet (&optional size)
  (declare (type (or null (signed-byte 32)) size))
  (let ((p (av-packet-alloc)))
    (when (null-pointer-p p)
      (error 'out-of-memory))
    (when size
      (let ((rv (av-new-packet p size)))
        (when (< rv 0)
          (%free-packet p)
          (check-rv rv))))
    (let ((pkt (%packet p)))
      (finalize pkt (lambda () (%free-packet p)))
      pkt)))

(declaim (inline packet-alive-p))
(defun packet-alive-p (packet)
  (declare (type packet packet))
  (not (null-pointer-p (%packet-ptr packet))))

(defun clone-packet (packet)
  (declare (type packet packet))
  (let ((ptr (av-packet-clone (%packet-ptr packet))))
    (when (null-pointer-p ptr)
      (error 'out-of-memory))
    (let ((pkt (%packet ptr)))
      (finalize pkt (lambda () (%free-packet ptr)))
      pkt)))

(defun free-packet (packet)
  (declare (type packet packet))
  (let ((ptr (%packet-ptr packet)))
    (unless (null-pointer-p ptr)
      (cancel-finalization packet)
      (%free-packet ptr)
      (setf (%packet-ptr packet) (null-pointer))))
  (values))

(defmacro with-packet ((var &optional size) &body body)
  `(let ((,var (make-packet ,size)))
     (declare (dynamic-extent ,var))
     (unwind-protect (locally ,@body)
       (free-packet ,var))))

(defun shrink-packet (packet size)
  (declare (type packet packet)
           (type (signed-byte 32) size))
  (with-packet-slots (ptr packet acc)
    (av-shrink-packet ptr size)))

(defun grow-packet (packet grow-by)
  (declare (type packet packet)
           (type (signed-byte 32) grow-by))
  (with-packet-slots (ptr packet acc)
    (check-rv (av-grow-packet ptr grow-by))))

(defun packet-rescale-ts (packet from to)
  (declare (type packet packet)
           (type rational from to))
  (with-packet-slots (ptr packet acc)
    (av-packet-rescale-ts ptr (mkrational from) (mkrational to))))

(declaim (inline packet-data))
(defun packet-data (packet)
  (declare (type packet packet))
  (with-packet-slots (ptr packet acc)
    (acc data)))

(declaim (inline packet-pts))
(defun packet-pts (packet)
  (declare (type packet packet))
  (with-packet-slots (ptr packet acc)
    (acc pts)))

(declaim (inline packet-dts))
(defun packet-dts (packet)
  (declare (type packet packet))
  (with-packet-slots (ptr packet acc)
    (acc dts)))

(declaim (inline packet-size))
(defun packet-size (packet)
  (declare (type packet packet))
  (with-packet-slots (ptr packet acc)
    (acc size)))

(declaim (inline packet-flags))
(defun packet-flags (packet)
  (declare (type packet packet))
  (with-packet-slots (ptr packet acc)
    (acc flags)))

(declaim (inline packet-duration))
(defun packet-duration (packet)
  (declare (type packet packet))
  (with-packet-slots (ptr packet acc)
    (acc duration)))

(declaim (inline packet-position))
(defun packet-position (packet)
  (declare (type packet packet))
  (with-packet-slots (ptr packet acc)
    (acc pos)))

(declaim (inline packet-stream-index))
(defun packet-stream-index (packet)
  (declare (type packet packet))
  (with-packet-slots (ptr packet acc)
    (acc stream-index)))

(declaim (inline (setf packet-pts)))
(defun (setf packet-pts) (pts packet)
  (declare (type packet packet)
           (type (signed-byte 64) pts))
  (with-packet-slots (ptr packet acc)
    (setf (acc pts) pts)))

(declaim (inline (setf packet-dts)))
(defun (setf packet-dts) (dts packet)
  (declare (type packet packet)
           (type (signed-byte 64) dts))
  (with-packet-slots (ptr packet acc)
    (setf (acc dts) dts)))

(declaim (inline (setf packet-flags)))
(defun (setf packet-flags) (flags packet)
  (declare (type packet packet)
           (type list flags))
  (with-packet-slots (ptr packet acc)
    (setf (acc flags) flags)))

(declaim (inline (setf packet-stream-index)))
(defun (setf packet-stream-index) (stream-index packet)
  (declare (type packet packet)
           (type (signed-byte 32) stream-index))
  (with-packet-slots (ptr packet acc)
    (setf (acc stream-index) stream-index)))

(declaim (inline (setf packet-position)))
(defun (setf packet-position) (pos packet)
  (declare (type packet packet)
           (type (signed-byte 64) pos))
  (with-packet-slots (ptr packet acc)
    (setf (acc pos) pos)))

(declaim (inline (setf packet-duration)))
(defun (setf packet-duration) (duration packet)
  (declare (type packet packet)
           (type (signed-byte 64) duration))
  (with-packet-slots (ptr packet acc)
    (setf (acc duration) duration)))

(defmethod print-object ((packet packet) stream)
  (if *print-pretty*
    (print-unreadable-object (packet stream :type t)
      (if (packet-alive-p packet)
        (pprint-logical-block (stream nil)
          (format stream "Data: #x~8,'0X; " (pointer-address (packet-data packet)))
          (pprint-newline :fill stream)
          (format stream "Size: ~a; " (packet-size packet))
          (pprint-newline :fill stream)
          (format stream "Stream index: ~a; " (packet-stream-index packet))
          (pprint-newline :fill stream)
          (let ((pts (packet-pts packet)))
            (unless (= pts +no-pts+)
              (format stream "PTS: ~a; " pts)
              (pprint-newline :fill stream)))
          (let ((dts (packet-dts packet)))
            (unless (= dts +no-pts+)
              (format stream "DTS: ~a; " dts)
              (pprint-newline :fill stream)))
          (format stream "Duration: ~a; " (packet-duration packet))
          (pprint-newline :fill stream)
          (format stream "Position: ~a; " (packet-position packet))
          (pprint-newline :fill stream)          
          (let ((flags (packet-flags packet)))
            (when flags
              (format stream "Flags: ~{~a~^, ~}" flags))))
        (write-string "Disposed" stream)))
    (call-next-method)))

;; vim: ft=lisp et
