;; An HTML page entirely in Scheme

(setq html-document.title "A Web Page entirely in Scheme!")

(@! html-document.body "append"
  (html-element "p" "This is an HTML page entirely in HTML")
  (html-element "p" "You can generate any HTML at all.")
  (html-element "ul"
    (html-element "li" "One")
    (html-element "li" "Two")
    (html-element "li" "Three")
    (for-of str ["Four" "Five "Six"]
      (html-element "li" (+ "See: " str "!"))
    )
  )
)