;; Web graphics in SchemeJS

;; Example adapted from https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D
(define (house gfx-context)
  (line-width 10)
  (stroke-rect 75 140 150 110) ;; Wall
  (fill-rect 130 190 40 60)    ;; Door
  ;; Roof
  (begin-path)
  (move-to 50 140)
  (line-to 150 60)
  (line-to 250 140)
  (close-path)
  (stroke)
)
(house (draw2d (canvas "house" 300 300)))

;; animation helper
(define (sinusoidal (? magnitude 1) (? period 1000))
  (* magnitude (sin (* *2pi* (/ (Date-now) period))))
)

;; Ship Arriving Too Late to Save a Drowning Witch
(define (satltsadw gfx-context)
  (gfx-save ;; saves then restores the graphics state, transforms etc
    (scale (/ (canvas-width) 10))
    (line-width .2)
    (stroke-rect 0 0 10 9) ;; bounding box
    (move-to 0 7) (line-to 10 7) (stroke) ;; sea level
    (begin-path) (rect 0 0 10 7) (clip) ;; clipping region for the ship and witch 
    (translate (sinusoidal .1 3000) (sinusoidal .4 4500))  ;; bob the ship and witch
    (move-to -5 3) (line-to 5.5 3) (line-to -0.5 11) ;; ship
    (translate (sinusoidal 0.05 1500) (sinusoidal 0.1 1337))  ;; bob the witch some more
    (move-to 4.5 9) (line-to 6.5 5) (line-to 8.5 9) ;; witch
    (stroke) ;; draw them
  )
  (font "80% sans-serif")
  (fill-text "Ship Arriving Too Late to Save a Drowning Witch" 0.1 (canvas-width))
)

(define shipCanvas (canvas "Too Late!" 300 320))
(@= shipCanvas 'draw satltsadw)
(@= shipCanvas 'animate true)