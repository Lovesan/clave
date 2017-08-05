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
  :altivec
  :sh4
  :simplearm
  (:ipp 13)
  (:xvid 14)
  (:xvidmmx 14)
  (:simplearmv5te 16)
  :simplearmv6
  :simplevis
  (:faan 20)
  (:simpleneon 22)
  (:simplealpha 23)
  (:simpleauto 128))

(defcenum thread-type
  (:frame 1)
  (:slice 2))

(defcstruct (av-codec-context :class av-codec-context)
  (av-class :pointer)
  (log-level-offset :int)
  (codec-type media-type)
  (codec codec-ptr)
  (codec-name :char :count 32) ;; deprecated
  (codec-id codec-id)
  (codec-tag :uint)
  (stream-codec-tag :uint)
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
  (me-method :int) ;; deprecated
  (draw-horiz-band :pointer)
  (get-format :pointer)
  (max-b-frames :int)
  (b-quant-factor :float)
  (rc-strategy :int) ;; deprecated
  (b-frame-strategy :int) ;; deprecated
  (b-quant-offset :float)
  (has-b-frames :boolean)
  (mpeg-quant :int) ;; deprecated
  (i-quant-factor :float)
  (i-quant-offset :float)
  (lumi-masking :float)
  (temporal-cplx-masking :float)
  (spatial-cplx-masking :float)
  (p-masking :float)
  (dark-masking :float)
  (slice-count :int)
  (prediction-method :int) ;; deprecated
  (slice-offset :pointer)
  (sample-aspect-ratio (:struct av-rational))
  (me-cmp ff-cmp)
  (me-sub-cmp ff-cmp)
  (mb-cmp ff-cmp)
  (ildct-cmp ff-cmp)
  (dia-size :int)
  (last-predictor-count :int)
  (pre-me :int) ;; deprecated
  (me-pre-cmp ff-cmp)
  (pre-dia-size :int)
  (me-subpel-quality :int)
  (dtg-active-format :int) ;; deprecated
  (me-range :int)
  (intra-quant-bias :int) ;; deprecated
  (inter-quant-bias :int) ;; deprecated
  (slice-flags slice-flags)
  (xvmc-acceleration :int) ;; deprecated
  (mb-decision mb-decision)
  (intra-matrix :pointer)
  (inter-matrix :pointer)
  (scenechange-threshold :int) ;; deprecated
  (noise-reduction :int) ;; deprecated
  (me-threshold :int) ;; deprecated
  (mb-threshold :int) ;; deprecated
  (intra-dc-precision :int)
  (skip-top :int)
  (skip-bottom :int)
  (border-masking :float) ;; deprecated
  (mb-lmin :int)
  (mb-lmax :int)
  (mb-penalty-compensation :int) ;; deprecated
  (bidir-refine :int)
  (brd-scale :int) ;; deprecated
  (keyint-min :int)
  (refs :int)
  (chromaoffset :int) ;; deprecated
  (scenechange-factor :int) ;; deprecated
  (mv0-threshold :int)
  (b-sensitivity :int) ;; deprecated
  (color-primaries color-primaries)
  (color-trc color-transfer-characteristic)
  (color-space color-space)
  (color-range color-range)
  (chroma-location chroma-location)
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
  (refcounted-frames :boolean)
  (qcompress :float)
  (qblur :float)
  (qmin :int)
  (qmax :int)
  (max-qdiff :int)
  (rc-qsquish :float) ;; deprecated
  (rc-qmod-amp :float) ;; deprecated
  (rc-qmod-freq :float) ;; deprecated
  (rc-buffer-size :int)
  (rc-override-count :int)
  (rc-override :pointer)
  (rc-eq :pointer) ;; deprecated
  (rc-max-rate :int64)
  (rc-min-rate :int64)
  (rc-buffer-aggressivity :float) ;; deprecated
  (rc-initial-cplx :float) ;; deprecated
  (rc-max-available-vbv-use :float)
  (rc-min-vbv-overflow-use :float)
  (rc-initial-buffer-occupancy :int)
  (coder-type :int) ;; deprecated
  (context-model :int) ;; deprecated
  (lmin :int) ;; deprecated
  (lmax :int) ;; deprecated
  (frame-skip-threshold :int) ;; deprecated
  (frame-skip-factor :int) ;; deprecated
  (frame-skip-exp :int) ;; deprecated
  (frame-skip-cmp :int) ;; deprecated
  (trellis :int)
  (min-prediction-order :int) ;; deprecated
  (max-prediction-order :int) ;; deprecated
  (timecode-frame-start :int64) ;; deprecated
  (rtp-callback :pointer) ;; deprecated
  (rtp-payload-size :int) ;; deprecated
  (mv-bits :int) ;; deprecated
  (header-bits :int) ;; deprecated
  (i-tex-bits :int) ;; deprecated
  (p-tex-bits :int) ;; deprecated
  (i-count :int) ;; deprecated
  (p-count :int) ;; deprecated
  (skip-count :int) ;; deprecated
  (misc-bits :int) ;; deprecated
  (frame-bits :int) ;; deprecated
  (stats-out :pointer)
  (stats-in :pointer)
  (workaround-bugs :int)
  (strict-std-compliance compliance)
  (error-concealment error-concealment)
  (debug :int) ;; TODO: enum
  (debug-mv :int) ;; TODO: enum
  (err-recognition :int)
  (reordered-opaque :int64)
  (hwaccel :pointer)
  (hwaccel-context :pointer)
  (errors :uint64 :count 8)
  (dct-algo dct-algo)
  (idct-algo idct-algo)
  (bits-per-coded-sample :int)
  (bits-per-raw-sample :int)
  (lowres :int)
  (coded-frame :pointer) ;; deprecated
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
  (error-rate :int) ;; deprecated
  (vbv-delay :uint64) ;; deprecated
  (side-data-only-packets :int) ;; deprecated
  (initial-padding :int)
  (frame-rate (:struct av-rational))
  (sw-pix-fmt pixel-format)
  (pkt-timebase-num :int)
  (pkt-timebase-denom :int)
  (codec-descriptor :pointer)
  (pts-correction-num-faulty-pts :int64)
  (pts-correction-num-faulty-dts :int64)
  (pts-correction-last-pts :int64)
  (pts-correction-last-dts :int64)
  (sub-charenc :pointer)
  (sub-charenc-mode :int)
  (skip-alpha :boolean)
  (seek-preroll :int)
  (chroma-intra-matrix :pointer)
  (dump-separator :pointer)
  (codec-whitelist :pointer)
  (properties :uint)
  (coded-side-data :pointer)
  (nb-coded-side-data :int)
  (hw-frames-ctx :pointer)
  (sub-text-format :int)
  (trailing-padding :int))

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
  (chroma-location keyword "Chroma location" t)
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
