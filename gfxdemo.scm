;; Web graphics in SchemeJS

;; Animation helper: a sinusoidal time-varying value
(define (sinusoidal (? magnitude 1) (? period 1) (? phase 0))
  (* magnitude (sin (+ phase (* *2pi* (/ (Date-now) period 1000)))))
)

;; Ship Arriving Too Late to Save a Drowning Witch
(define (satltsadw gfx-context)
  (gfx-save ;; saves then restores the graphics state, transforms etc
    (scale (/ (canvas-width) 10)) ;; scale to a 10 x 10 coordinate system
    (line-width .2)
    (stroke-rect 0 0 10 9) ;; bounding box
    (move-to 0 7) (line-to 10 7) (stroke) ;; sea level
    (begin-path) (rect 0 0 10 7) (clip) ;; clipping region for the ship and witch 
    (translate (sinusoidal .1 3) (sinusoidal .4 4.5))  ;; bob the ship and witch
    (move-to -5 3) (line-to 5.5 3) (line-to -0.5 11) ;; ship
    (translate (sinusoidal 0.05 1.5) (sinusoidal 0.1 1.337))  ;; bob the witch some more
    (move-to 4.5 9) (line-to 6.5 5) (line-to 8.5 9) ;; witch
    (stroke) ;; draw them
  )
  (font "80% sans-serif")
  (fill-text "Ship Arriving Too Late to Save a Drowning Witch" 0.1 (canvas-width))
)

(define shipCanvas (canvas "Too Late!" 300 320))
(@= shipCanvas 'draw satltsadw)
(@= shipCanvas 'animate true)
(@! shipCanvas 'show 20 20)