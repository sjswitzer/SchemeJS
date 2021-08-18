;;
;; Media Demo
;;

(define (o-scope gfx-context)
  (let ( (time-domain-data (@ microphoneCanvas 'time-domain-data))
         (frequency-domain-data (@ microphoneCanvas 'frequency-domain-data)) )
    (gfx-save  ;; Draw spectrum
      (fill-style "yellow")
      ;; Scale to the number of samples + one (one is the width of the rectangles) along the x axis,
      ;; and 256 (the range of values) along the y axis.
      (scale (/ (canvas-width) (+ 1 (@ frequency-domain-data "length"))) (/ (canvas-height) 256))
      (for-in i value frequency-domain-data
        (fill-rect i (- 256 value) 2 value) ;; make one pixel wider to overdraw a bit and not leave gaps
      )
    )
    (gfx-save  ;; Draw waveform
      (stroke-style "green") (line-width 3) (begin-path)
      ;; Scale to the number of samples along the x axis,
      ;; and 256 along the y axis.
      (scale (/ (canvas-width) (+ 1 (@ time-domain-data "length"))) (/ (canvas-height) 256))
      (for-in i value time-domain-data
        (line-to i value)
      )
      (stroke)
    )
  )
)

(define microphoneCanvas (canvas "Microphone"  600 400))
(@= microphoneCanvas 'draw o-scope)
(@= microphoneCanvas 'animate true)
(@= microphoneCanvas 'user-media { "audio": true })
(@= microphoneCanvas 'clear-color "black")

