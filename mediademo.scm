;;
;; Media Demo
;;

(compile (o-scope gfx-context)
  (let ( (time-domain-data (@ o-scope-canvas 'time-domain-data))
         (frequency-domain-data (@ o-scope-canvas 'frequency-domain-data)) )
    (gfx-save  ;; Draw spectrum
      ;; Scale to the number of samples + one (one is the width of the rectangles) along the x axis,
      ;; and 256 (the range of values) along the y axis.
      (scale (/ (canvas-width) (+ 1 (@ frequency-domain-data "length"))) (/ (canvas-height) 256))
      (fill-style "#a22")
      (for-in i value frequency-domain-data
        (fill-rect i (- 256 value) 2 value) ;; make one pixel wider to overdraw a bit and not leave gaps
      )
    )
    (gfx-save  ;; Draw waveform
      ;; Scale to the number of samples along the x axis,
      ;; and 256 along the y axis.
      (scale (/ (canvas-width) (+ 1 (@ time-domain-data "length"))) (/ (canvas-height) 256))
      (stroke-style "#8f8") (begin-path)
      (for-in i value time-domain-data
        (line-to i value)
      )
      (stroke)
    )
  )
)

(def o-scope-canvas (canvas "Oscilloscope"  800 300))
(@= o-scope-canvas 'draw o-scope)
(@= o-scope-canvas 'animate true)
(@= o-scope-canvas 'user-media { "audio": true })
(@= o-scope-canvas 'clear-color "black")

;; Here's what o-scope compiles into
(println (String o-scope))

