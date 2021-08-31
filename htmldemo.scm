;;
;; Demonstrates HTML document API
;;

;; Open a new browser window
;;   If you have a popup blocker, allow it and try again
(def window (@! browser-window "open" ""))

(def html-document window.document)

(setq html-document.title "Hello World!")

(@! html-document.body "append"
  (html-element "p" "Hello world!")
)