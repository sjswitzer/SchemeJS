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

;; Ship Arriving Too Late to Save a Drowning Witch
(define (satltsadw gfx-context)
  (gfx-save
    (scale (/ (canvas-width) 10))
    (line-width .2)
    (stroke-rect 0 0 10 9) ;; bounding box
    (move-to 0 7) (line-to 10 7) (stroke) ;; sea level
    (move-to 0 3) (line-to 5.5 3) (line-to 2.5 7) (stroke) ;; ship
    (move-to 5.5 7) (line-to 6.5 5) (line-to 7.5 7) (stroke) ;; witch
  )
  (font "80% sans-serif")
  (fill-text "Ship Arriving Too Late to Save a Drowning Witch" 0 (canvas-width))
)
(satltsadw (draw2d (canvas "Too late" 300 320)))