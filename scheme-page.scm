;; An HTML page almost entirely in Scheme

(setq document.title "A Web Page entirely in Scheme!")

(@! document.body "append"
  (html-element "p" "This is an HTML page entirely in HTML")
  (html-element "p" "You can generate any HTML at all.")
  (html-element "ul"
    (html-element "li" "One")
    (html-element "li" "Two")
    (html-element "li" "Three")
    ...(map (\[str] (html-element "li" (+ "See: " str "!"))) ["Four" "Five" "Six"]
    )
  )
)