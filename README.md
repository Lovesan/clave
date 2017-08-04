# Common Lisp Audio Video Engine - Lisp FFmpeg interface

### Work in progress

### Current FFmpeg version - 3.2.4

## Simple example - convert media file to .mp3

```common-lisp

(defun convert-audio-file (infile outfile)
  (declare (type string infile outfile)
           (optimize (debug 3)))
  (clave:with-input-context (input infile)
    (clave:find-stream-info input)
    (clave:dump-format input :url infile)
    (let* ((stream (find-if (lambda (s) (eq :audio (clave:codecpar-codec-type
                                                    (clave:media-stream-codecpar s))))
                            (clave:format-context-streams input)))
           (codecpar (and stream (clave:media-stream-codecpar stream)))
           (encoder (clave:find-encoder :mp3)))
      (unless codecpar (error "No audio stream found"))
      (clave:with-codec-context (inctx (clave:find-decoder
                                        (clave:codecpar-codec-id codecpar))
                                       codecpar)
        (clave:open-codec-context inctx)
        (clave:with-codec-context (outctx encoder)
          (setf (clave:codec-context-sample-format outctx)
                (clave:codecpar-format codecpar)
                (clave:codec-context-channels outctx)
                (clave:codecpar-channels codecpar)
                (clave:codec-context-sample-rate outctx)
                (clave:codecpar-sample-rate codecpar)
                (clave:codec-context-channel-layout outctx)
                (clave:codecpar-channel-layout codecpar)
                (clave:codec-context-global-quality outctx)
                1)
            (clave:open-codec-context outctx encoder)
          (clave:with-output-context (output :filename outfile)
            (let* ((outstream (clave:add-media-stream output encoder))
                   (outpar (clave:media-stream-codecpar outstream)))
              (clave:parameters-from-context outpar outctx)
              (clave:init-output output)
              (clave:write-header output)
              (clave:dump-format output :url outfile)
              (clave:with-packet (packet)
                (clave:with-frame (frame)
                  (loop :while (clave:read-frame input packet)
                        :when (= (clave:media-stream-index stream)
                                 (clave:packet-stream-index packet)) :do
                          (clave:send-packet inctx packet)
                          (loop :while (clave:receive-frame inctx frame) :do
                            (clave:send-frame outctx frame)
                            (loop :while (clave:receive-packet outctx packet) :do
                              (clave:write-frame output packet))))
                  (clave:send-flush-frame outctx)
                  (loop :while (clave:receive-packet outctx packet) :do
                    (clave:write-frame output packet))
                  (clave:write-trailer output))))))))))

```

## TODO

* swresample & swscale
* Missing codec-context/format-context/media-stream slots
* Documentation
* Math & other utils
