;;
;; Demonstrates HTML document API
;;

;; Open a new browser window
;;   If you have a popup blocker, allow it and try again
(def window (@! browser-window "open" ""))

(def document window.document)

(setq document.title "Hello World!")

(@! document.body "append"
  (html-element "p" "Hello world!")
)