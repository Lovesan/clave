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

(defcfun (av-strerror "av_strerror" :library libavutil)
    :int
  (code :int)
  (buf :pointer)
  (size size-t))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mktag (a b c d)    
    (declare (type (or character (unsigned-byte 8)) a b c d))
    (when (characterp a) (setf a (logand #xFF (char-code a))))
    (when (characterp b) (setf b (logand #xFF (char-code b))))
    (when (characterp c) (setf c (logand #xFF (char-code c))))
    (when (characterp d) (setf d (logand #xFF (char-code d))))
    (logand #xFFFFFFFF (logior a (ash b 8) (ash c 16) (ash d 24))))
  (defun fferrtag (a b c d)
    (declare (type (or character (unsigned-byte 8)) a b c d))
    (with-foreign-pointer (p 4)
      (setf (mem-ref p :uint32) (mktag a b c d))
      (- (mem-ref p :int32)))))

(declaim (ftype (function (t) base-string)
                ffmpeg-error-string))

(defun report-ffmpeg-error (ffmpeg-error stream)
  (declare (type ffmpeg-error ffmpeg-error))
  (format stream
          "~s signalled: ~a"
          (class-name (class-of ffmpeg-error))
          (ffmpeg-error-string ffmpeg-error))
  ffmpeg-error)

(define-condition ffmpeg-error (error)
  ((code :accessor ffmpeg-error-code
         :initarg :code
         :initform -1))
  (:report report-ffmpeg-error))

(defun ffmpeg-error-string (ffmpeg-error)
  (declare (type ffmpeg-error ffmpeg-error))
  (with-foreign-pointer (buf 128)
    (coerce
     (if (zerop (av-strerror (ffmpeg-error-code ffmpeg-error) buf 128))
       (foreign-string-to-lisp buf :encoding :utf-8)
       "Unidentified error")
     'base-string)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +bsf-not-found+ (fferrtag #xF8 #\B #\S #\F))
  (defconstant +ffmpeg-bug+ (fferrtag #\B #\U #\F #\!))
  (defconstant +buffer-too-small+ (fferrtag #\B #\U #\F #\S))
  (defconstant +decoder-not-found+ (fferrtag #xF8 #\D #\E #\C))
  (defconstant +demuxer-not-found+ (fferrtag #xF8 #\D #\E #\M))
  (defconstant +encoder-not-found+ (fferrtag #xF8 #\E #\N #\C))
  (defconstant +ffmpeg-eof+ (fferrtag #\E #\O #\F #\Space))
  (defconstant +ffmpeg-exit+ (fferrtag #\E #\X #\I #\T))
  (defconstant +external-error+ (fferrtag #\E #\X #\T #\Space))
  (defconstant +filter-not-found+ (fferrtag #xF8 #\F #\I #\L))
  (defconstant +invalid-data+ (fferrtag #\I #\N #\D #\A))
  (defconstant +muxer-not-found+ (fferrtag #xF8 #\M #\U #\X))
  (defconstant +option-not-found+ (fferrtag #xF8 #\O #\P #\T))
  (defconstant +patches-welcome+ (fferrtag #\P #\A #\W #\E))
  (defconstant +protocol-not-found+ (fferrtag #xF8 #\P #\R #\O))
  (defconstant +stream-not-found+ (fferrtag #xF8 #\S #\T #\R))
  (defconstant +io-error+ -5)
  (defconstant +not-available+ -11)
  (defconstant +out-of-memory+ -12)
  (defconstant +permission-denied+ -13)
  (defconstant +invalid-argument+ -22))

(define-condition bsf-not-found (ffmpeg-error)
  ()
  (:default-initargs :code +bsf-not-found+))
(define-condition ffmpeg-bug (ffmpeg-error)
  ()
  (:default-initargs :code +ffmpeg-bug+))
(define-condition buffer-too-small (ffmpeg-error)
  ()
  (:default-initargs :code +buffer-too-small+))
(define-condition decoder-not-found (ffmpeg-error)
  ()
  (:default-initargs :code +decoder-not-found+))
(define-condition demuxer-not-found (ffmpeg-error)
  ()
  (:default-initargs :code +demuxer-not-found+))
(define-condition encoder-not-found (ffmpeg-error)
  ()
  (:default-initargs :code +encoder-not-found+))
(define-condition ffmpeg-eof (ffmpeg-error end-of-file)
  ()
  (:default-initargs :code +ffmpeg-eof+))
(define-condition ffmpeg-exit (ffmpeg-error)
  ()
  (:default-initargs :code +ffmpeg-exit+))
(define-condition external-error (ffmpeg-error)
  ()
  (:default-initargs :code +external-error+))
(define-condition filter-not-found (ffmpeg-error)
  ()
  (:default-initargs :code +filter-not-found+))
(define-condition invalid-data (ffmpeg-error)
  ()
  (:default-initargs :code +invalid-data+))
(define-condition muxer-not-found (ffmpeg-error)
  ()
  (:default-initargs :code +muxer-not-found+))
(define-condition option-not-found (ffmpeg-error)
  ()
  (:default-initargs :code +option-not-found+))
(define-condition patches-welcome (ffmpeg-error)
  ()
  (:default-initargs :code +patches-welcome+))
(define-condition protocol-not-found (ffmpeg-error)
  ()
  (:default-initargs :code +protocol-not-found+))
(define-condition stream-not-found (ffmpeg-error)
  ()
  (:default-initargs :code +stream-not-found+))
(define-condition io-error (ffmpeg-error)
  ()
  (:default-initargs :code +io-error+))
(define-condition not-available (ffmpeg-error)
  ()
  (:default-initargs :code +not-available+))
(define-condition out-of-memory (ffmpeg-error)
  ()
  (:default-initargs :code +out-of-memory+))
(define-condition permission-denied (ffmpeg-error)
  ()
  (:default-initargs :code +permission-denied+))
(define-condition invalid-argument (ffmpeg-error)
  ()
  (:default-initargs :code +invalid-argument+))

(defmacro check-rv (code)
  (with-gensyms (rv)
    `(let ((,rv ,code))
       (when (< ,rv 0)
         (error (case ,rv
                  (#.+bsf-not-found+ 'bsf-not-found)
                  (#.+ffmpeg-bug+ 'ffmpeg-bug)
                  (#.+buffer-too-small+ 'buffer-too-small)
                  (#.+decoder-not-found+ 'decoder-not-found)
                  (#.+demuxer-not-found+ 'demuxer-not-found)
                  (#.+encoder-not-found+ 'encoder-not-found)
                  (#.+ffmpeg-exit+ 'ffmpeg-exit)
                  (#.+ffmpeg-eof+ 'ffmpeg-eof)
                  (#.+external-error+ 'external-error)
                  (#.+filter-not-found+ 'filter-not-found)
                  (#.+invalid-data+ 'invalid-data)
                  (#.+muxer-not-found+ 'muxer-not-found)
                  (#.+option-not-found+ 'option-not-found)
                  (#.+patches-welcome+ 'patches-welcome)
                  (#.+protocol-not-found+ 'protocol-not-found)
                  (#.+io-error+ 'io-error)
                  (#.+not-available+ 'not-available)
                  (#.+out-of-memory+ 'out-of-memory)
                  (#.+permission-denied+ 'permission-denied)
                  (#.+invalid-argument+ 'invalid-argument)
                  (T 'ffmpeg-error))
                :code ,rv))
       ,rv)))

;; vim: ft=lisp et
