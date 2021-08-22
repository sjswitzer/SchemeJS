;;
;; Web graphics in SchemeJS
;;
;; The Web Graphics primitives are simply the JavaScript CanvasRenderingContext2D
;; methods (https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D)
;; mapped into SchemeJS with camelCase replaced-by-dashes and implicitly applied
;; to the variable named "gfx-context".
;; So "(move-to x y)" equates to "gfxContext.moveTo(x, y)" in JavaScript.
;; 
;; The exceptions are the stashing functions, described inline below, and "gfx-save".
;; "(gfx-save expr expr ...)" saves the current drawing state (colors, transforms,
;; line widths, etc.) then executes the enclosed expressions. When done, it restores
;; the previous drawing state. So you can take a drawing excursion, with whatever scales,
;; rotations, colors, line widths you want then return to the previous state and continue.
;;
;; Finally, the "gfxContext.save()" and "gfxContext ctx.restore()" functions are renamed
;; "(save-context)" and "(restore-context)". "Save" and "restore" are confusingly
;; general, and you won't be needing them anyway because you can do the two
;; together in a structured way by using
;;   (gfx-save expr expr ...)
;; which is the same as
;;   (save-context) expr expr ... (restore-context)
;; but can't be messed up.
;;

;; Animation helper: a sinusoidal time-varying value of a given magintude,
;; period (in seconds) and phase.
;; (Date.now) is the number of milliseconds since midnight Jan 1, 1970 (the Unix epoch).
(compile (sinusoidal (magnitude 1) (period 1) (phase 0))
  (* magnitude (sin (+ phase (* 2 *pi* (/ (Date-now) period 1000)))))
)

;; Ship Arriving Too Late to Save a Drowning Witch
(compile (satltsadw gfx-context)
  (gfx-save ;; Saves then restores the graphics state (scale, colors, etc) afterwards
    (scale (/ (canvas-width) 10))  ;; Scale to a 10 x 10 coordinate system
    (line-width .2)
    (stroke-rect 0 0 10 9)  ;; Bounding box
    (move-to 0 7) (line-to 10 7) (stroke)  ;; Sea level
    (begin-path) (rect 0 0 10 7) (clip)  ;; Clipping region for the ship and witch 
    (translate (sinusoidal .1 3) (sinusoidal .4 4.5))  ;; Bob the ship and witch
    (move-to -5 3) (line-to 5.5 3) (line-to -0.5 11)  ;; Ship
    (translate (sinusoidal 0.05 1.5) (sinusoidal 0.1 1.337))  ;; Bob the witch some more
    (move-to 4.5 9) (line-to 6.5 5) (line-to 8.5 9)  ;; Witch
    (stroke)  ;; draw them
  )
  (font "80% sans-serif")
  (fill-text "Ship Arriving Too Late to Save a Drowning Witch" 0.1 (canvas-width))
)

(def ship-canvas (canvas "Too Late!" 300 320))
(@= ship-canvas 'draw satltsadw)  ;; Set the draw function
(@= ship-canvas 'animate true)    ;; Enable animation; otherwise just draws initially and when resized
(@! ship-canvas 'show 20 20)      ;; Move the canvas widget from its default position

;; Now a lissajous figure
(compile (lissajous gfx-context tick)
  ;; Fade the canvas by drawing over it with black and a very low alpha every several ticks
  ;; If the alpha is too low, things won't fade things completely; they just converge on light
  ;; gray. So instead use a slightly higer alpha and only apply it every several frames.
  (fill-style "#00000004")
  (? (== 0 (% tick 5))  ;; "%" is remainder, so this is true every 5 ticks
    (fill-rect 0 0 (canvas-width) (canvas-height))
  )
  ;; Scale to a 100 x 100 coordinate system and move the origin to the center
  (scale (/ (canvas-width) 100) (/ (canvas-height) 100))
  (translate 50 50)
  ;; "Stashing" is a gimmick to save a location from one iteration to the next,
  ;; useful for connecting lines:
  (move-to-stashed)
  (stash-point (sinusoidal 45 2.19) (sinusoidal 45 1.53))
  (line-to-stashed)
  (stroke-style "green") (line-width 1.5) (stroke)
)

(def lissajous-canvas (canvas "Lissajous"  300 300))
(@= lissajous-canvas 'draw lissajous)         ;; Set the draw function
(@= lissajous-canvas 'animate true)           ;; Enable animation
(@! lissajous-canvas 'show 300 30)            ;; Move the canvas widget from its default position
(@! lissajous-canvas 'backing-buffer 600 600) ;; Use an offscreen-backing buffer
(@= lissajous-canvas 'clear-canvas false)     ;; And don't clear the canvas before re-drawing
(@= lissajous-canvas 'clear-color "black")    ;; Well, except initially

;; What it looks like compiled
(println (String lissajous))

;; Spirograph
(compile (spirograph gfx-context tick)
  (let ((angle (* 2 *pi* (/ (Date-now) 1000 period))))
    ;; Fade the canvas by drawing over it with white and a very low alpha every several ticks
    (fill-style "#ffffff0c")
    (? (== 0 (% tick 30))
      (fill-rect 0 0 (canvas-width) (canvas-height))
    )
    ;; Scale to a 100 x 100 coordinate system with the origin in the center
    (scale (/ (canvas-width) 100) (/ (canvas-height) 100))
    (translate 50 50)
    (move-to-stashed)
    (stash-point
      (+ (* r1 (cos angle)) (* r3 (cos (* angle (- (/ r1 r2))))))
      (+ (* r1 (sin angle)) (* r3 (sin (* angle (- (/ r1 r2)))))))
    (line-to-stashed)
    (stroke-style sc) (stroke)
  )
)
(def period 2)  ;; You can play with these numbers in the console!
(def r1 25)
(def r2 14)
(def r3 20)
(def sc "magenta")

(def spirograph-canvas (canvas "Spirograph"  300 300))
(@= spirograph-canvas 'draw spirograph)
(@= spirograph-canvas 'animate true)
(@! spirograph-canvas 'show 600 40)
(@= spirograph-canvas 'clear-canvas false)

;; What it looks like compiled
(println (String spirograph))

;; Here's a widget to scribble in interactively
(def scribble (canvas "Scribble" 300))

;; Set the gfx-context variable to the canvas's drawing context so you can now
;; enter drawing commands into the command window and draw interactively.
;; Again, the commands are all documented at
;;    https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D
;; Try it!
(def gfx-context (canvas-gfx-context scribble))
(stroke-style "blue")
(stroke-rect 10 10 40 40)
(fill-text "Try it! Just enter drawing commands below." 10 65)

