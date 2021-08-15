;; Web graphics in SchemeJS

;; Example adapted from https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D
(define (house (? title "house")) (draw2d (canvas title 300 300)
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
))

(house)