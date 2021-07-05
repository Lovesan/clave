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

(defconstant +ff-lambda-max+ (1- (* 256 128)))
(defconstant +num-data-pointers+ 8)

(defcenum ff-cmp
  :sad
  :sse
  :satd
  :dct
  :psnr
  :bit
  :rd
  :zero
  :vsad
  :vsse
  :nsse
  :w53
  :w97
  :dctmax
  :dct264
  :median-sad
  (:chroma 256))

(defbitfield slice-flags
  (:coded-order 1)
  (:allow-field 2)
  (:allow-plane 4))

(defcenum mb-decision
  :simple
  :bits
  :rd)

(defcenum error-concealment
  (:guess-mvs 1)
  (:deblock 2)
  (:favor-inter 256))

(defcenum dct-algo
  :auto
  :fastint
  :int
  :mmx
  :altivec
  :faan)

(defcenum idct-algo
  :auto
  :int
  :simple
  :simplemmx
  (:arm 7)
  (:altivec 8)
  (:simplearm 10)
  (:xvid 14)
  (:simplearmv5te 16)
  :simplearmv6
  (:faan 20)
  (:simpleneon 22)
  (:none 24)
  (:simpleauto 128))

(defcenum thread-type
  (:frame 1)
  (:slice 2))

(defcenum debug
  (:pict-info 1)
  (:rc 2)
  (:bitstream 4)
  (:mb-type 8)
  (:qp 16))

(defcstruct (av-codec-context :class av-codec-context)
  (av-class :pointer)
  (log-level-offset :int)
  (codec-type media-type)
  (codec codec-ptr)
  (codec-id codec-id)
  (codec-tag :uint)
  (priv-data :pointer)
  (internal :pointer)
  (opaque :pointer)
  (bit-rate :int64)
  (bit-rate-tolerance :int)
  (global-quality :int)
  (compression-level :int)
  (flags codec-flags)
  (flags2 :int)
  (extradata :pointer)
  (extradata-size :int)
  (time-base (:struct av-rational))
  (ticks-per-frame :int)
  (delay :int)
  (width :int)
  (height :int)
  (coded-width :int)
  (coded-height :int)
  (gop-size :int)
  (pixel-format pixel-format)
  (draw-horiz-band :pointer)
  (get-format :pointer)
  (max-b-frames :int)
  (b-quant-factor :float)
  #-FF-API-PRIVATE-OPT (b-frame-strategy :int) ;; deprecated
  (b-quant-offset :float)
  (has-b-frames :boolean)
  #-FF-API-PRIVATE-OPT (mpeg-quant :int) ;; deprecated
  (i-quant-factor :float)
  (i-quant-offset :float)
  (lumi-masking :float)
  (temporal-cplx-masking :float)
  (spatial-cplx-masking :float)
  (p-masking :float)
  (dark-masking :float)
  (slice-count :int)
  #-FF-API-PRIVATE-OPT (prediction-method :int) ;; deprecated
  (slice-offset :pointer)
  (sample-aspect-ratio (:struct av-rational))
  (me-cmp ff-cmp)
  (me-sub-cmp ff-cmp)
  (mb-cmp ff-cmp)
  (ildct-cmp ff-cmp)
  (dia-size :int)
  (last-predictor-count :int)
  #-FF-API-PRIVATE-OPT (pre-me :int) ;; deprecated
  (me-pre-cmp ff-cmp)
  (pre-dia-size :int)
  (me-subpel-quality :int)
  (me-range :int)
  (slice-flags slice-flags)
  (mb-decision mb-decision)
  (intra-matrix :pointer)
  (inter-matrix :pointer)
  #-FF-API-PRIVATE-OPT (scenechange-threshold :int) ;; deprecated
  #-FF-API-PRIVATE-OPT (noise-reduction :int) ;; deprecated
  (intra-dc-precision :int)
  (skip-top :int)
  (skip-bottom :int)
  (mb-lmin :int)
  (mb-lmax :int)
  #-FF-API-PRIVATE-OPT (mb-penalty-compensation :int) ;; deprecated
  (bidir-refine :int)
  #-FF-API-PRIVATE-OPT (brd-scale :int) ;; deprecated
  (keyint-min :int)
  (refs :int)
  #-FF-API-PRIVATE-OPT (chromaoffset :int) ;; deprecated
  (mv0-threshold :int)
  #-FF-API-PRIVATE-OPT (b-sensitivity :int) ;; deprecated
  (color-primaries color-primaries)
  (color-trc color-transfer-characteristic)
  (color-space color-space)
  (color-range color-range)
  (chroma-sample-location chroma-location)
  (slices :int)
  (field-order field-order)
  (sample-rate :int)
  (channels :int)
  (sample-fmt sample-format)
  (frame-size :int)
  (frame-number :int)
  (block-align :int)
  (cutoff :int)
  (channel-layout channels-or-layout)
  (request-channel-layout channels-or-layout)
  (audio-service-type :int)
  (request-sample-format sample-format)
  (get-buffer2 :pointer)
  (refcounted-frames :boolean) ;; newly deprecated
  (qcompress :float)
  (qblur :float)
  (qmin :int)
  (qmax :int)
  (max-qdiff :int)
  (rc-buffer-size :int)
  (rc-override-count :int)
  (rc-override :pointer)
  (rc-max-rate :int64)
  (rc-min-rate :int64)
  (rc-max-available-vbv-use :float)
  (rc-min-vbv-overflow-use :float)
  (rc-initial-buffer-occupancy :int)
  #-FF-API-CODER-TYPE (coder-type :int) ;; deprecated
  #-FF-API-PRIVATE-OPT (context-model :int) ;; deprecated
  #-FF-API-PRIVATE-OPT (frame-skip-threshold :int) ;; deprecated
  #-FF-API-PRIVATE-OPT (frame-skip-factor :int) ;; deprecated
  #-FF-API-PRIVATE-OPT (frame-skip-exp :int) ;; deprecated
  #-FF-API-PRIVATE-OPT (frame-skip-cmp :int) ;; deprecated
  (trellis :int)
  #-FF-API-PRIVATE-OPT (min-prediction-order :int) ;; deprecated
  #-FF-API-PRIVATE-OPT (max-prediction-order :int) ;; deprecated
  #-FF-API-PRIVATE-OPT (timecode-frame-start :int64) ;; deprecated
  #-FF-API-RTP-CALLBACK (rtp-callback :pointer) ;; deprecated
  #-FF-API-PRIVATE-OPT (rtp-payload-size :int) ;; deprecated
  #-FF-API-STAT-BITS (mv-bits :int) ;; deprecated
  #-FF-API-STAT-BITS (header-bits :int) ;; deprecated
  #-FF-API-STAT-BITS (i-tex-bits :int) ;; deprecated
  #-FF-API-STAT-BITS (p-tex-bits :int) ;; deprecated
  #-FF-API-STAT-BITS (i-count :int) ;; deprecated
  #-FF-API-STAT-BITS (p-count :int) ;; deprecated
  #-FF-API-STAT-BITS (skip-count :int) ;; deprecated
  #-FF-API-STAT-BITS (misc-bits :int) ;; deprecated
  #-FF-API-STAT-BITS (frame-bits :int) ;; deprecated
  (stats-out :pointer)
  (stats-in :pointer)
  (workaround-bugs :int)
  (strict-std-compliance compliance)
  (error-concealment error-concealment)
  (debug debug) ;; TODO: enum
  #+FF-API-DEBUG-MV (debug-mv :int) ;; TODO: enum
  (err-recognition :int)
  (reordered-opaque :int64)
  (hwaccel :pointer)
  (hwaccel-context :pointer)
  (errors :uint64 :count #.+num-data-pointers+)
  (dct-algo dct-algo)
  (idct-algo idct-algo)
  (bits-per-coded-sample :int)
  (bits-per-raw-sample :int)
  #+FF-API-LOWRES (lowres :int)
  #-FF-API-CODED-FRAME (coded-frame :pointer) ;; deprecated
  (thread-count :int)
  (thread-type thread-type)
  (active-thread-type thread-type)
  (thread-safe-callbacks :boolean)
  (execute :pointer)
  (execute2 :pointer)
  (nsse-weight :int)
  (profile :int)
  (level :int)
  (skip-loop-filter discard)
  (skip-idct discard)
  (skip-frame discard)
  (subtitle-header :pointer)
  (subtitle-header-size :int)
  #-FF-API-VBV-DELAY (vbv-delay :uint64) ;; deprecated
  #-FF-API-SIDE-DATA-ONLY-PKT (side-data-only-packets :int) ;; deprecated
  (initial-padding :int)
  (frame-rate (:struct av-rational))
  (sw-pix-fmt pixel-format)
  (pkt-timebase (:struct av-rational))
  (codec-descriptor :pointer)
  #-FF-API-LOWRES (lowres :int)
  (pts-correction-num-faulty-pts :int64)
  (pts-correction-num-faulty-dts :int64)
  (pts-correction-last-pts :int64)
  (pts-correction-last-dts :int64)
  (sub-charenc :pointer)
  (sub-charenc-mode :int)
  (skip-alpha :boolean)
  (seek-preroll :int)
  #-FF-API-DEBUG-MV (debug-mv :int) ;; TODO: enum
  (chroma-intra-matrix :pointer)
  (dump-separator :pointer)
  (codec-whitelist :pointer)
  (properties :uint)
  (coded-side-data :pointer)
  (nb-coded-side-data :int)
  (hw-frames-ctx :pointer)
  (sub-text-format :int)
  (trailing-padding :int)
  (max-pixels :int64)
  (hw-device-ctx :pointer)
  (hw-accel-flags :int)
  (apply-cropping :int)
  (extra-hw-frames :int)
  (discard-damaged-percentage :int)
  (max-samples :int64)
  (export-side-date :int))

(defcfun (avcodec-alloc-context3 "avcodec_alloc_context3" :library libavcodec)
    :pointer
  (codec :pointer))

(defcfun (avcodec-free-context "avcodec_free_context" :library libavcodec)
    :void
  (pp :pointer))

(defcfun (avcodec-open2 "avcodec_open2" :library libavcodec)
    :int
  (ctx :pointer)
  (codec :pointer)
  (dict :pointer))

(defcfun (avcodec-is-open "avcodec_is_open" :library libavcodec)
    :boolean
  (ctx :pointer))

(defcfun (avcodec-send-packet "avcodec_send_packet" :library libavcodec)
    :int
  (ctx :pointer)
  (packet :pointer))

(defcfun (avcodec-receive-packet "avcodec_receive_packet" :library libavcodec)
    :int
  (ctx :pointer)
  (packet :pointer))

(defcfun (avcodec-send-frame "avcodec_send_frame" :library libavcodec)
    :int
  (ctx :pointer)
  (frame :pointer))

(defcfun (avcodec-receive-frame "avcodec_receive_frame" :library libavcodec)
    :int
  (ctx :pointer)
  (frame :pointer))

(defaccessors codec-context codec-context- (:struct av-codec-context) %codec-context-ptr
  (codec-id keyword "Codec id" t)
  (codec-type keyword "Codec media type" t)
  (codec (or null codec) "Codec" t)
  (flags (or keyword list) "Codec context flags" t)
  (bit-rate int64 "Bit rate" t)
  (bit-rate-tolerance int "Bit rate tolerance" t)
  (global-quality int "Global quality (1 = MAX)" t)
  (compression-level int "Compression level" t)
  (time-base rational "Time base" t)
  (ticks-per-frame int "Ticks per frame" t)
  (delay int "Codec delay" t)
  (width int "Frame width" t)
  (height int "Frame height" t)
  (gop-size int "GOP size" t)
  (pixel-format keyword "Pixel format" t)
  (max-b-frames int "Max B frames" t)
  ((has-b-frames-p has-b-frames) boolean "Has B frames?" t)
  (slice-count int "Slice count" t)
  (sample-aspect-ratio rational "Sample aspect ratio" t)
  (slice-flags (or keyword list) "Slice flags" t)
  (refs int "Reference frame count" t)
  (color-primaries keyword "Color primaries" t)
  (color-trc keyword "Color transfer characteristics" t)
  (color-space keyword "Color space" t)
  (color-range keyword "Color range" t)
  (slices int "Slices" t)
  (field-order keyword "Field order" t)
  (sample-rate int "Sample rate" t)
  (channels int "Channel count" t)
  ((sample-format sample-fmt) keyword "Sample format" t)
  (frame-size int "Audio frame sample count")
  (frame-number int "Frame number")
  (channel-layout (or keyword list) "Channel layout" t)
  ((req-channel-layout request-channel-layout) (or keyword list) "Requested channel layout" t)
  (qcompress single-float "Quality: compression" t)
  (qblur single-float "Quality: blur" t)
  (qmin int "Quality minimum" t)
  (qmax int "Quality maximum" t)
  (max-qdiff int "Quality max diff" t)
  ((min-rate rc-min-rate) int64 "Min RC rate" t)
  ((max-rate rc-max-rate) int64 "Max RC rate" t)
  (bits-per-coded-sample int "Bits per coded sample" t)
  (bits-per-raw-sample int "Bits per raw sample" t)
  ((compliance strict-std-compliance) keyword "Codec standard compliance" t)
  (hwaccel-context foreign-pointer "HWAccel context" t)
  (thread-count int "Max thread count" t)
  ((thread-safe-callbacks-p thread-safe-callbacks) t "Are callbacks thread safe" t)
  ((profile-id profile) int "Codec profile id" t)
  (level int "Codec level" t)
  (frame-rate rational "Frame rate" t)
  )

(declaim (inline %free-codec-ctx))
(defun %free-codec-ctx (p)
  (declare (type foreign-pointer p))
  (with-foreign-object (pp :pointer)
    (setf (mem-ref pp :pointer) p)
    (avcodec-free-context pp))
  (values))

(defun free-codec-context (ctx)
  (declare (type codec-context ctx))
  (let ((ptr (%codec-context-ptr ctx)))
    (unless (null-pointer-p ptr)
      (%free-codec-ctx ptr)
      (setf (%codec-context-ptr ctx) (null-pointer))
      (cancel-finalization ctx))
    (values)))

(defun codec-context-alive-p (ctx)
  (declare (type codec-context ctx))
  (not (null-pointer-p (%codec-context-ptr ctx))))

(defun codec-context-open-p (ctx)
  (declare (type codec-context ctx))
  (avcodec-is-open (%codec-context-ptr ctx)))

(defun make-codec-context (&optional codec codecpar)
  (declare (type (or null codec) codec)
           (type (or null codec-parameters) codecpar))
  (let ((ptr (avcodec-alloc-context3
              (if codec (%codec-ptr codec) (null-pointer)))))
    (when (null-pointer-p ptr)
      (error 'out-of-memory))
    (let ((ctx (%codec-context ptr)))
      (finalize ctx (lambda () (%free-codec-ctx ptr)))
      (when codecpar
        (parameters-to-context ctx codecpar))
      ctx)))

(defmacro with-codec-context ((var &rest args) &body body)
  `(let ((,var (make-codec-context ,@args)))
     (unwind-protect (locally ,@body)
       (free-codec-context ,var))))

(defun open-codec-context (ctx &optional codec options)
  (declare (type codec-context ctx)
           (type (or null codec) codec)
           (type list options))
  (with-dict (dict options)
    (check-rv (avcodec-open2 (%codec-context-ptr ctx)
                             (if codec (%codec-ptr codec) (null-pointer))
                             dict))
    (from-dict dict)))

(defun send-packet (ctx packet)
  (declare (type codec-context ctx)
           (type packet packet))
  (let ((rv (avcodec-send-packet (%codec-context-ptr ctx)
                                 (%packet-ptr packet))))
    (if (< rv 0)
      (case rv
        ((#.+ffmpeg-eof+ #.+not-available+) nil)
        (t (check-rv rv) nil))
      t)))

(defun send-flush-packet (ctx)
  (declare (type codec-context ctx))
  (let ((rv (avcodec-send-packet (%codec-context-ptr ctx)
                                 (null-pointer))))
    (if (< rv 0)
      (case rv
        ((#.+ffmpeg-eof+ #.+not-available+) nil)
        (t (check-rv rv) nil))
      t)))

(defun receive-packet (ctx packet)
  (declare (type codec-context ctx)
           (type packet packet))
  (let ((rv (avcodec-receive-packet (%codec-context-ptr ctx)
                                    (%packet-ptr packet))))
    (if (< rv 0)
      (case rv
        ((#.+ffmpeg-eof+ #.+not-available+) nil)
        (t (check-rv rv) nil))
      t)))

(defun send-frame (ctx frame)
  (declare (type codec-context ctx)
           (type frame frame))
  (let ((rv (avcodec-send-frame (%codec-context-ptr ctx)
                                (%frame-ptr frame))))
    (if (< rv 0)
      (case rv
        ((#.+ffmpeg-eof+ #.+not-available+) nil)
        (t (check-rv rv) nil))
      t)))

(defun send-flush-frame (ctx)
  (declare (type codec-context ctx))
  (let ((rv (avcodec-send-frame (%codec-context-ptr ctx)
                                (null-pointer))))
    (if (< rv 0)
      (case rv
        ((#.+ffmpeg-eof+ #.+not-available+) nil)
        (t (check-rv rv) nil))
      t)))

(defun receive-frame (ctx frame)
  (declare (type codec-context ctx)
           (type frame frame))
  (let ((rv (avcodec-receive-frame (%codec-context-ptr ctx)
                                   (%frame-ptr frame))))
    (if (< rv 0)
      (case rv
        ((#.+ffmpeg-eof+ #.+not-available+) nil)
        (t (check-rv rv) nil))
      t)))

(defmethod print-object ((ctx codec-context) stream)
  (print-unreadable-object (ctx stream :type t :identity t)
    (if (codec-context-alive-p ctx)
      (pprint-logical-block (stream nil)
        (with-codec-context-slots
            (codec flags bit-rate
             global-quality time-base delay width
             height pixel-format sample-format
             sample-rate channels frame-size channel-layout
             profile-id level frame-rate)
            ctx
          (write-string "Codec: " stream)
          (write codec :stream stream)
          (pprint-newline :mandatory stream)
          (when flags
            (format stream "Flags: ~{~a~^, ~:_~}~:@_" flags))
          (when (> bit-rate 0)
            (format stream "Bit rate: ~a~:@_" bit-rate))
          (when (> global-quality 0)
            (format stream "Global quality: ~a~:@_" global-quality))
          (when (/= time-base 0)
            (format stream "Time base: ~a~:@_" time-base))
          (when (/= delay 0)
            (format stream "Delay: ~a~:@_" delay))
          (when (/= profile-id 0)
            (format stream "Profile: ~a~:@_" profile-id))
          (when (/= level 0)
            (format stream "Level: ~a~:@_" level))
          (when (> width 0)
            (format stream "Pixel format: ~a~:@_" pixel-format)
            (format stream "Frame width: ~a~:@_" width)
            (format stream "Frame height: ~a~:@_" height)
            (format stream "Frame rate: ~a~:@_" (float frame-rate)))
          (when (> sample-rate 0)
            (format stream "Sample format: ~a~:@_" sample-format)
            (format stream "Channels: ~a~:@_" channels)
            (format stream "Channel layout: ~a~:@_" channel-layout)
            (format stream "Sample rate: ~a~:@_" sample-rate)
            (format stream "Audio frame size: ~a~:@_" frame-size))))
      (format stream "Closed ")))
  ctx)

;; vim: ft=lisp et
