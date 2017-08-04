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

(defcenum avoid-negative-ts
  (:auto -1)
  (:make-non-negative 1)
  (:make-zero 2))

(defcenum duration-estimation-method
  :pts
  :stream
  :bitrate)

(defcstruct av-format-context
  (av-class :pointer)
  (iformat :pointer)
  (oformat :pointer)
  (priv-data :pointer)
  (pb :pointer)
  (no-header :boolean) ;; ctx-flags, 1 == no-header
  (nb-streams :uint)
  (streams :pointer)
  (filename :char :count 1024)
  (start-time :int64)
  (duration :int64)
  (bit-rate :int64)
  (packet-size :uint)
  (max-delay :int)
  (flags format-context-flags)
  (probesize :int64)
  (max-analyze-duration :int64)
  (key :pointer)
  (keylen :int)
  (nb-programs :uint)
  (programs :pointer)
  (video-codec-id codec-id)
  (audio-codec-id codec-id)
  (subtitle-codec-id codec-id)
  (max-index-size :uint)
  (max-picture-buffer :uint)
  (nb-chapters :uint)
  (chapters :pointer)
  (metadata :pointer)
  (start-time-realtime :int64)
  (fps-probe-size :int)
  (error-recognition :int)
  (icb-callback :pointer)
  (icb-opaque :pointer)
  (debug :boolean)
  (max-interleave-delta :int64)
  (strict-std-compliance :int)
  (metadata-updated :boolean)
  (max-ts-probe :int)
  (avoid-negative-ts avoid-negative-ts)
  (ts-id :int)
  (audio-preload :int)
  (max-chunk-duration :int)
  (max-chunk-size :int)
  (use-wallclock-as-timestamps :boolean)
  (avio-flags io-flags)
  (duration-estimation-method duration-estimation-method)
  (skip-initial-bytes :int64)
  (correct-ts-overflow :uint)
  (seek2any :boolean)
  (flush-packets :boolean)
  (probe-score :int)
  (format-probesize :int)
  (codec-whitelist :pointer)
  (format-whitelist :pointer)
  (internal :pointer)
  (io-repositioned :int)
  (video-codec :pointer)
  (audio-codec :pointer)
  (subtitle-codec :pointer)
  (data-codec :pointer)
  (metadata-header-padding :int)
  (opaque :pointer)
  (control-message-cb :pointer)
  (output-ts-offset :int64)
  (dump-separator :pointer)
  (data-codec-id codec-id)
  (open-cb :pointer)
  (protocol-whitelist :pointer)
  (io-open :pointer)
  (io-close :pointer)
  (protocol-blacklist :pointer)
  (max-streams :int))

(defmacro with-format-context-slots ((var ctx accessor-name) &body body)
  `(let ((,var (%format-context-ptr ,ctx)))
     (declare (ignorable ,var))
     (macrolet ((,accessor-name (slot)
                  `(foreign-slot-value ,',var '(:struct av-format-context) ',slot)))
       ,@body)))

(defcfun (avformat-alloc-context "avformat_alloc_context" :library libavformat)
    :pointer)

(defcfun (avformat-alloc-output-context "avformat_alloc_output_context2" :library libavformat)
    :int
  (pp :pointer)
  (oformat :pointer)
  (name :pointer)
  (filename :pointer))

(defcfun (avformat-open-input "avformat_open_input" :library libavformat)
    :int
  (pp :pointer)
  (url :pointer)
  (iformat :pointer)
  (opts :pointer))

(defcfun (avformat-free-context "avformat_free_context" :library libavformat)
    :void
  (p :pointer))

(defcfun (avformat-close-input "avformat_close_input" :library libavformat)
    :void
  (pp :pointer))

(defcallback format-interrupt-callback :boolean ((opaque :pointer))
  (let ((ctx (get-fmt-ctx (pointer-address opaque))))
    (when ctx
      (let ((icb (%format-context-icb ctx)))
        (when icb (funcall icb))))))

(declaim (inline format-context-input-p))
(defun format-context-input-p (ctx)
  (declare (type format-context ctx))
  (%format-context-input ctx))

(defun free-format-context (ctx)
  (let ((io (%format-context-io ctx))
        (ptr (%format-context-ptr ctx)))
    (unless (null-pointer-p ptr)
      (unless io
        (avio-closep (foreign-slot-pointer ptr '(:struct av-format-context) 'pb)))
      (avformat-free-context ptr)
      (setf (%format-context-ptr ctx) (null-pointer))
      (cancel-finalization ctx)))
  (values))

(defcfun (av-sdp-create "av_sdp_create" :library libavformat)
    :int
  (ps :pointer)
  (len :int)
  (buf :pointer)
  (size :int))

(defun create-sdp (&rest format-contexts)
  (let* ((len (length format-contexts))
         (ps (av-mallocz (* len (foreign-type-size :pointer)))))
    (unwind-protect
         (with-foreign-pointer (buf 2048)
           (loop :for i :of-type int32 :from 0 :below len
                 :for ctx :in format-contexts
                 :do (setf (mem-aref ps :pointer i)
                           (%format-context-ptr ctx))
                 :finally (check-rv (av-sdp-create ps len buf 2048))
                          (return (remove #\Return (foreign-string-to-lisp buf :encoding :utf-8)))))
      (av-free ps))))

(defun format-context-alive-p (ctx)
  (declare (type format-context ctx))
  (not (null-pointer-p (%format-context-ptr ctx))))

(defun open-input-context (url &key format
                                    io-context
                                    interrupt-callback
                                    flags
                                    options)
  (declare (type string url)
           (type (or null io-format) format)
           (type (or null io-context) io-context)
           (type (or null symbol function) interrupt-callback)
           (type list options)
           (type (or list keyword) flags))
  (mklistf flags)
  (with-foreign-objects ((pp :pointer))
    (with-dict (dict options)
      (let ((p (avformat-alloc-context))
            (iformat (if format (%io-format-ptr format) (null-pointer))))
        (when (null-pointer-p p) (error 'out-of-memory))
        (when io-context
          (push :custom-io flags)
          (setf (foreign-slot-value p '(:struct av-format-context) 'pb)
                (%io-context-ptr io-context)))
        (setf (mem-ref pp :pointer) p
              (foreign-slot-value p '(:struct av-format-context) 'flags) flags
              (foreign-slot-value p '(:struct av-format-context) 'opaque) p)
        (when interrupt-callback
          (setf (foreign-slot-value p '(:struct av-format-context) 'icb-opaque)
                p
                (foreign-slot-value p '(:struct av-format-context) 'icb-callback)
                (callback format-interrupt-callback)))
        (with-foreign-pointer (purl 1024)
          (lisp-string-to-foreign url purl 1024 :encoding :utf-8)
          (check-rv (avformat-open-input pp purl iformat dict)))
        (let ((ctx (%format-context p t io-context interrupt-callback))
              (custom-io (and io-context t)))
          (set-fmt-ctx (pointer-address p) ctx)
          (finalize ctx (lambda ()
                          (unless custom-io
                            (avio-closep (foreign-slot-pointer p '(:struct av-format-context) 'pb)))
                          (avformat-free-context p)))
          (values ctx (from-dict dict)))))))

(defun make-output-context (&key filename
                                 format
                                 io-context
                                 flags
                                 format-name)
  (declare (type (or null string) filename format-name)
           (type (or null io-format) format)
           (type (or null io-context) io-context)
           (type (or list keyword) flags))
  (mklistf flags)
  (when (not (or io-context filename))
    (error "Either :IO-CONTEXT or :FILENAME should be present"))
  (with-foreign-pointer (pfilename 1024)
    (when filename
      (lisp-string-to-foreign filename pfilename 1024 :encoding :utf-8))
    (with-foreign-pointer (pname 1024)
      (when format-name
        (lisp-string-to-foreign format-name pname 1024 :encoding :utf-8))
      (with-foreign-object (pp :pointer)
        (setf (mem-ref pp :pointer) (null-pointer))
        (check-rv (avformat-alloc-output-context
                   pp
                   (if format (%io-format-ptr format) (null-pointer))
                   (if format-name pname (null-pointer))
                   (if filename pfilename (null-pointer))))
        (let ((p (mem-ref pp :pointer)))
          (if io-context
            (setf flags
                  (cons :custom-io flags)
                  (foreign-slot-value p '(:struct av-format-context) 'pb)
                  (%io-context-ptr io-context))
            (let ((rv (avio-open2 (foreign-slot-pointer p '(:struct av-format-context) 'pb)
                                  pfilename
                                  :write
                                  (null-pointer)
                                  (null-pointer))))
              (when (< rv 0)
                (avformat-free-context p)
                (check-rv rv))))
          (setf (foreign-slot-value p '(:struct av-format-context) 'flags) flags)
          (let ((ctx (%format-context p nil io-context nil))
                (custom-io (and io-context t)))
            (finalize ctx (lambda ()
                            (unless custom-io
                              (avio-closep (foreign-slot-pointer p '(:struct av-format-context) 'pb)))
                            (avformat-free-context p)))
            ctx))))))

(defmacro with-input-context ((var &rest args) &body body)
  `(let ((,var (open-input-context ,@args)))
     (unwind-protect (locally ,@body)
       (free-format-context ,var))))

(defmacro with-output-context ((var &rest args) &body body)
  `(let ((,var (make-output-context ,@args)))
     (unwind-protect (locally ,@body)
       (free-format-context ,var))))

(defun format-context-streams (ctx)
  (declare (type format-context ctx))
  (with-format-context-slots (ptr ctx acc)
    (let ((n (acc nb-streams))
          (p (acc streams))
          (list '()))
      (dotimes (i n)
        (push (%media-stream (mem-aref p :pointer i) ctx) list))
      (nreverse list))))

(defun format-context-format (ctx)
  (declare (type format-context ctx))
  (with-format-context-slots (ptr ctx acc)
    (let* ((input (%format-context-input ctx))
           (ptr (if input (acc iformat) (acc oformat))))
      (if (null-pointer-p ptr) nil (%io-format ptr input)))))

(defun format-context-filename (ctx)
  (declare (type format-context ctx))
  (with-format-context-slots (ptr ctx acc)
    (foreign-string-to-lisp (acc filename) :encoding :utf-8)))

(defun format-context-flags (ctx)
  (declare (type format-context ctx))  
  (with-format-context-slots (ptr ctx acc)
    (acc flags)))

(defcfun (avformat-new-stream "avformat_new_stream" :library libavformat)
    :pointer
  (ctx :pointer)
  (codec :pointer))

(defun add-media-stream (ctx codec)
  (declare (type format-context ctx)
           (type (or null codec) codec))
  (let ((p (avformat-new-stream (%format-context-ptr ctx)
                                (if codec
                                  (%codec-ptr codec)
                                  (null-pointer)))))
    (when (null-pointer-p p)
      (error 'out-of-memory))
    (%media-stream p ctx)))

(defcfun (avformat-find-stream-info "avformat_find_stream_info" :library libavformat)
    :int
  (ctx :pointer)
  (dict :pointer))

(defun find-stream-info (ctx &optional options)
  (declare (type format-context ctx)
           (type list options))
  (with-dict (dict options)
    (check-rv (avformat-find-stream-info (%format-context-ptr ctx) dict))
    (from-dict dict)))

(defcfun (av-dump-format "av_dump_format" :library libavformat)
    :void
  (ctx :pointer)
  (index :int)
  (url :pointer)
  (is-output :boolean))

(defun dump-format (ctx &key (index 0) (url ""))
  (declare (type string url)
           (type int32 index)
           (type format-context ctx))
  (with-foreign-pointer (purl 512)
    (lisp-string-to-foreign url purl 512 :encoding :utf-8)
    (av-dump-format (%format-context-ptr ctx)
                    index
                    purl
                    (not (%format-context-input ctx)))
    (values)))

(defcfun (avformat-init-output "avformat_init_output" :library libavformat)
    :int
  (ctx :pointer)
  (dict :pointer))

(defun init-output (ctx &optional options)
  (declare (type format-context ctx)
           (type list options))
  (with-dict (dict options)
    (check-rv (avformat-init-output (%format-context-ptr ctx) dict))
    (from-dict dict)))

(defcfun (avformat-write-header "avformat_write_header" :library libavformat)
    :int
  (ctx :pointer)
  (dict :pointer))

(defun write-header (ctx &optional options)
  (declare (type format-context ctx)
           (type list options))
  (with-dict (dict options)
    (check-rv (avformat-write-header (%format-context-ptr ctx) dict))
    (from-dict dict)))

(defcfun (av-write-trailer "av_write_trailer" :library libavformat)
    :int
  (ctx :pointer))

(defun write-trailer (ctx)
  (declare (type format-context ctx))
  (check-rv (av-write-trailer (%format-context-ptr ctx)))
  (values))

(defcfun (av-read-frame "av_read_frame" :library libavformat)
    :int
  (ctx :pointer)
  (packet :pointer))

(defun read-frame (ctx packet)
  (declare (type format-context ctx)
           (type packet packet))
  (let ((rv (av-read-frame (%format-context-ptr ctx)
                           (%packet-ptr packet))))
    (if (< rv 0)
      (case rv
        ((#.+not-available+ #.+ffmpeg-eof+) nil)
        (t (check-rv rv)))
      t)))

(defcfun (av-write-frame "av_write_frame" :library libavformat)
    :int
  (ctx :pointer)
  (packet :pointer))

(defun write-frame (ctx packet)
  (declare (type format-context ctx)
           (type packet packet))
  (let ((rv (av-write-frame (%format-context-ptr ctx)
                            (%packet-ptr packet))))
    (if (< rv 0)
      (case rv
        ((#.+not-available+ #.+ffmpeg-eof+) nil)
        (t (check-rv rv)))
      t)))

(defcfun (av-interleaved-write-frame "av_interleaved_write_frame" :library libavformat)
    :int
  (ctx :pointer)
  (packet :pointer))

(defun interleaved-write-frame (ctx packet)
  (declare (type format-context ctx)
           (type packet packet))
  (let ((rv (av-interleaved-write-frame
             (%format-context-ptr ctx)
             (%packet-ptr packet))))
    (if (< rv 0)
      (case rv
        ((#.+not-available+ #.+ffmpeg-eof+) nil)
        (t (check-rv rv)))
      t)))

(defmethod print-object ((ctx format-context) stream)
  (print-unreadable-object (ctx stream :identity t :type t)
    (if (format-context-alive-p ctx)
      (pprint-logical-block (stream nil)
        (let-when (filename (format-context-filename ctx)
                            (string/= filename ""))
          (format stream "Filename: ~a~:@_" filename))
        (let-when (flags (format-context-flags ctx))
          (format stream "Flags: ~{~a~^, ~:_~}~:@_" flags))
        (let-when (format (format-context-format ctx))
          (write-string "Format: " stream)
          (write format :stream stream)
          (pprint-newline :mandatory stream)))
      (format stream "Closed ")))
  ctx)


;; vim: ft=lisp et
