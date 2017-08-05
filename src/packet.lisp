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

(defcstruct (av-packet :class av-packet)
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

(defmacro with-packet ((var &rest args) &body body)
  `(let ((,var (make-packet ,@args)))
     (declare (dynamic-extent ,var))
     (unwind-protect (locally ,@body)
       (free-packet ,var))))

(defun shrink-packet (packet size)
  (declare (type packet packet)
           (type (signed-byte 32) size))
  (av-shrink-packet (%packet-ptr packet) size))

(defun grow-packet (packet grow-by)
  (declare (type packet packet)
           (type (signed-byte 32) grow-by))
  (check-rv (av-grow-packet (%packet-ptr packet) grow-by)))

(defun packet-rescale-ts (packet from to)
  (declare (type packet packet)
           (type rational from to))
  (av-packet-rescale-ts (%packet-ptr packet)
                        (mkrational from)
                        (mkrational to)))

(defaccessors packet packet- (:struct av-packet) %packet-ptr
  (data foreign-pointer "Packet data pointer" t)
  (pts int64 "Packet PTS" t)
  (dts int64 "Packet DTS" t)
  (size int64 "Packet size" t)
  (flags (or list keyword) "Packet flags" t)
  (stream-index int "Packet stream index" t)
  (duration int64 "Packet duration" t)
  ((position pos) int64 "Packet position" t))


(defun make-packet (&key size
                         (pts +no-pts+)
                         (dts +no-pts+)
                         flags
                         (stream-index 0)
                         (duration 0)
                         (position -1))
  (declare (type (or null int) size)
           (type int stream-index)
           (type (or keyword list) flags)
           (type int64 pts dts duration position))
  (let ((p (av-packet-alloc)))
    (when (null-pointer-p p)
      (error 'out-of-memory))
    (when size
      (let ((rv (av-new-packet p size)))
        (when (< rv 0)
          (%free-packet p)
          (check-rv rv))))
    (let ((packet (%packet p)))
      (finalize packet (lambda () (%free-packet p)))
      (setf (packet-flags packet) flags
            (packet-pts packet) pts
            (packet-dts packet) dts
            (packet-stream-index packet) stream-index
            (packet-duration packet) duration
            (packet-position packet) position)
      packet)))

(defmethod print-object ((packet packet) stream)
  (if *print-pretty*
    (print-unreadable-object (packet stream :type t :identity t)
      (if (packet-alive-p packet)
        (pprint-logical-block (stream nil)
          (format stream "Data: #x~8,'0X; ~:_" (pointer-address (packet-data packet)))
          (format stream "Size: ~a; ~:_" (packet-size packet))
          (format stream "Stream index: ~a; ~:_" (packet-stream-index packet))
          (let-when (pts (packet-pts packet) (/= pts +no-pts+))
            (format stream "PTS: ~a; ~:_" pts))
          (let-when (dts (packet-dts packet) (/= dts +no-pts+))
            (format stream "DTS: ~a; ~:_" dts))
          (format stream "Duration: ~a; ~:_" (packet-duration packet))
          (format stream "Position: ~a; ~:_" (packet-position packet))
          (let-when (flags (packet-flags packet))
            (format stream "Flags: ~{~a~^, ~}" flags)))
        (write-string "Disposed" stream)))
    (call-next-method)))

;; vim: ft=lisp et
