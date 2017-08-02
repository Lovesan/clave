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

(defcenum codec-id
  :NONE

  ;; video codecs
  :MPEG1VIDEO
  :MPEG2VIDEO ;;; preferred ID for MPEG-1/2 video decoding
  :MPEG2VIDEO-XVMC
  :H261
  :H263
  :RV10
  :RV20
  :MJPEG
  :MJPEGB
  :LJPEG
  :SP5X
  :JPEGLS
  :MPEG4
  :RAWVIDEO
  :MSMPEG4V1
  :MSMPEG4V2
  :MSMPEG4V3
  :WMV1
  :WMV2
  :H263P
  :H263I
  :FLV1
  :SVQ1
  :SVQ3
  :DVVIDEO
  :HUFFYUV
  :CYUV
  :H264
  :INDEO3
  :VP3
  :THEORA
  :ASV1
  :ASV2
  :FFV1
  :4XM
  :VCR1
  :CLJR
  :MDEC
  :ROQ
  :INTERPLAY-VIDEO
  :XAN-WC3
  :XAN-WC4
  :RPZA
  :CINEPAK
  :WS-VQA
  :MSRLE
  :MSVIDEO1
  :IDCIN
  :8BPS
  :SMC
  :FLIC
  :TRUEMOTION1
  :VMDVIDEO
  :MSZH
  :ZLIB
  :QTRLE
  :TSCC
  :ULTI
  :QDRAW
  :VIXL
  :QPEG
  :PNG
  :PPM
  :PBM
  :PGM
  :PGMYUV
  :PAM
  :FFVHUFF
  :RV30
  :RV40
  :VC1
  :WMV3
  :LOCO
  :WNV1
  :AASC
  :INDEO2
  :FRAPS
  :TRUEMOTION2
  :BMP
  :CSCD
  :MMVIDEO
  :ZMBV
  :AVS
  :SMACKVIDEO
  :NUV
  :KMVC
  :FLASHSV
  :CAVS
  :JPEG2000
  :VMNC
  :VP5
  :VP6
  :VP6F
  :TARGA
  :DSICINVIDEO
  :TIERTEXSEQVIDEO
  :TIFF
  :GIF
  :DXA
  :DNXHD
  :THP
  :SGI
  :C93
  :BETHSOFTVID
  :PTX
  :TXD
  :VP6A
  :AMV
  :VB
  :PCX
  :SUNRAST
  :INDEO4
  :INDEO5
  :MIMIC
  :RL2
  :ESCAPE124
  :DIRAC
  :BFI
  :CMV
  :MOTIONPIXELS
  :TGV
  :TGQ
  :TQI
  :AURA
  :AURA2
  :V210X
  :TMV
  :V210
  :DPX
  :MAD
  :FRWU
  :FLASHSV2
  :CDGRAPHICS
  :R210
  :ANM
  :BINKVIDEO
  :IFF-ILBM
  (:IFF-BYTERUN1 137)
  :KGV1
  :YOP
  :VP8
  :PICTOR
  :ANSI
  :A64-MULTI
  :A64-MULTI5
  :R10K
  :MXPEG
  :LAGARITH
  :PRORES
  :JV
  :DFA
  :WMV3IMAGE
  :VC1IMAGE
  :UTVIDEO
  :BMV-VIDEO
  :VBLE
  :DXTORY
  :V410
  :XWD
  :CDXL
  :XBM
  :ZEROCODEC
  :MSS1
  :MSA1
  :TSCC2
  :MTS2
  :CLLC
  :MSS2
  :VP9
  :AIC
  :ESCAPE130
  :G2M
  :WEBP
  :HNM4-VIDEO
  :HEVC
  (:H265 174)
  :FIC
  :ALIAS-PIX
  :BRENDER-PIX
  :PAF-VIDEO
  :EXR
  :VP7
  :SANM
  :SGIRLE
  :MVC1
  :MVC2
  :HQX
  :TDSC
  :HQ-HQA
  :HAP
  :DDS
  :DXV
  :SCREENPRESSO
  :RSCC
  
  (:Y41P #x8000)
  :AVRP
  :012V
  :AVUI
  :AYUV
  :TARGA-Y216
  :V308
  :V408
  :YUV4
  :AVRN
  :CPIA
  :XFACE
  :SNOW
  :SMVJPEG
  :APNG
  :DAALA
  :CFHD
  :TRUEMOTION2RT
  :M101
  :MAGICYUV
  :SHEERVIDEO
  :YLC
  
  ;; various PCM "codecs"
  (:PCM-S16LE #x10000)
  (:FIRST-AUDIO #x10000)     ;;; A dummy id pointing at the start of audio codecs
  :PCM-S16BE
  :PCM-U16LE
  :PCM-U16BE
  :PCM-S8
  :PCM-U8
  :PCM-MULAW
  :PCM-ALAW
  :PCM-S32LE
  :PCM-S32BE
  :PCM-U32LE
  :PCM-U32BE
  :PCM-S24LE
  :PCM-S24BE
  :PCM-U24LE
  :PCM-U24BE
  :PCM-S24DAUD
  :PCM-ZORK
  :PCM-S16LE-PLANAR
  :PCM-DVD
  :PCM-F32BE
  :PCM-F32LE
  :PCM-F64BE
  :PCM-F64LE
  :PCM-BLURAY
  :PCM-LXF
  :S302M
  :PCM-S8-PLANAR
  :PCM-S24LE-PLANAR
  :PCM-S32LE-PLANAR
  :PCM-S16BE-PLANAR
  
  (:PCM-S64LE #x10800)
  :PCM-S64BE
  
  ;; various ADPCM codecs
  (:ADPCM-IMA-QT #x11000)
  :ADPCM-IMA-WAV
  :ADPCM-IMA-DK3
  :ADPCM-IMA-DK4
  :ADPCM-IMA-WS
  :ADPCM-IMA-SMJPEG
  :ADPCM-MS
  :ADPCM-4XM
  :ADPCM-XA
  :ADPCM-ADX
  :ADPCM-EA
  :ADPCM-G726
  :ADPCM-CT
  :ADPCM-SWF
  :ADPCM-YAMAHA
  :ADPCM-SBPRO-4
  :ADPCM-SBPRO-3
  :ADPCM-SBPRO-2
  :ADPCM-THP
  :ADPCM-IMA-AMV
  :ADPCM-EA-R1
  :ADPCM-EA-R3
  :ADPCM-EA-R2
  :ADPCM-IMA-EA-SEAD
  :ADPCM-IMA-EA-EACS
  :ADPCM-EA-XAS
  :ADPCM-EA-MAXIS-XA
  :ADPCM-IMA-ISS
  :ADPCM-G722
  :ADPCM-IMA-APC
  :ADPCM-VIMA
  (:VIMA 69662)
  (:ADPCM-AFC #x11800)
  :ADPCM-IMA-OKI
  :ADPCM-DTK
  :ADPCM-IMA-RAD
  :ADPCM-G726LE
  :ADPCM-THP-LE
  :ADPCM-PSX
  :ADPCM-AICA
  :ADPCM-IMA-DAT4
  :ADPCM-MTAF
  
  ;; AMR
  (:AMR-NB #x12000)
  :AMR-WB
  
  ;; RealAudio codecs
  (:RA-144 #x13000)
  :RA-288
  
  ;; various DPCM codecs
  (:ROQ-DPCM #x14000)
  :INTERPLAY-DPCM
  :XAN-DPCM
  :SOL-DPCM
  
  (:SDX2-DPCM #x14800)
  
  ;; audio codecs
  (:MP2 #x15000)
  :MP3 ;;; preferred ID for decoding MPEG audio layer 1, 2 or 3
  :AAC
  :AC3
  :DTS
  :VORBIS
  :DVAUDIO
  :WMAV1
  :WMAV2
  :MACE3
  :MACE6
  :VMDAUDIO
  :FLAC
  :MP3ADU
  :MP3ON4
  :SHORTEN
  :ALAC
  :WESTWOOD-SND1
  :GSM ;;; as in Berlin toast format
  :QDM2
  :COOK
  :TRUESPEECH
  :TTA
  :SMACKAUDIO
  :QCELP
  :WAVPACK
  :DSICINAUDIO
  :IMC
  :MUSEPACK7
  :MLP
  :GSM-MS ;;; as found in WAV
  :ATRAC3
  :VOXWARE
  :APE
  :NELLYMOSER
  :MUSEPACK8
  :SPEEX
  :WMAVOICE
  :WMAPRO
  :WMALOSSLESS
  :ATRAC3P
  :EAC3
  :SIPR
  :MP1
  :TWINVQ
  :TRUEHD
  :MP4ALS
  :ATRAC1
  :BINKAUDIO-RDFT
  :BINKAUDIO-DCT
  :AAC-LATM
  :QDMC
  :CELT
  :G723-1
  :G729
  :8SVX-EXP
  :8SVX-FIB
  :BMV-AUDIO
  :RALF
  :IAC
  :ILBC
  :OPUS
  :COMFORT-NOISE
  :TAK
  :METASOUND
  :PAF-AUDIO
  :ON2AVC
  :DSS-SP
  
  (:FFWAVESYNTH #x15800)
  :SONIC
  :SONIC-LS
  :EVRC
  :SMV
  :DSD-LSBF
  :DSD-MSBF
  :DSD-LSBF-PLANAR
  :DSD-MSBF-PLANAR
  :4GV
  :INTERPLAY-ACM
  :XMA1
  :XMA2
  :DST
  
  ;; subtitle codecs
  (:DVD-SUBTITLE #x17000)
  (:FIRST-SUBTITLE #x17000)          ;;; A dummy ID pointing at the start of subtitle codecs.
  :DVB-SUBTITLE
  :TEXT  ;;; raw UTF-8 text
  :XSUB
  :SSA
  :MOV-TEXT
  :HDMV-PGS-SUBTITLE
  :DVB-TELETEXT
  :SRT
  
  (:MICRODVD #x17800)
  :EIA-608
  :JACOSUB
  :SAMI
  :REALTEXT
  :STL
  :SUBVIEWER1
  :SUBVIEWER
  :SUBRIP
  :WEBVTT
  :MPL2
  :VPLAYER
  :PJS
  :ASS
  :HDMV-TEXT-SUBTITLE
  
  ;; other specific kind of codecs (generally used for attachments)
  (:TTF #x18000)
  (:FIRST-UNKNOWN #x18000)           ;;; A dummy ID pointing at the start of various fake codecs.
  
  :SCTE-35 ;;; Contain timestamp estimated through PCR of program stream.
  (:BINTEXT #x18800)
  :XBIN
  :IDF
  :OTF
  :SMPTE-KLV
  :DVD-NAV
  :TIMED-ID3
  :BIN-DATA
  
  
  (:PROBE #x19000) ;;; codec_id is not known (like :NONE) but lavf should attempt to identify it
  
  (:MPEG2TS #x20000) ;;;< _FAKE_ codec to indicate a raw MPEG-2 TS
                         ;;;      stream (only used by libavformat)
  (:MPEG4SYSTEMS #x20001) ;;;< _FAKE_ codec to indicate a MPEG-4 Systems
                               ;;; stream (only used by libavformat)
  (:FFMETADATA #x21000)   ;;; Dummy codec for streams containing only metadata information.
  (:WRAPPED-AVFRAME #x21001) ;;; Passthrough codec, AVFrames wrapped in AVPacket
  )

;; vim: ft=lisp et
