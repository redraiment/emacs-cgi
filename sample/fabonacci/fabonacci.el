#!/usr/local/bin/elisp

;; A sample to show the usage of cgi.el

(require 'cgi)

(cgi/cookie "n"
 (or (cgi/param "n")
     (cgi/cookie "n")
     "15"))

(html
 (head
  (title "Fabonacci 1 -&gt; n")
  (meta (http-equiv . "Content-Type")
        (content . "text/html; charset=UTF-8")))
 (body
  (form (method . post)
   (span "n = ")
   (input (type . text) (name . n))
   (button (type . submit) "Submit"))
  (table (border . 1) (width . "100%")
   (thead
    (caption "Fabonacci")
    (tr
     (th "#")
     (th "Value")))
   (tbody
    <%
    (let ((a 0) (b 1) x)
      (dotimes (i (string-to-number
                   (cgi/cookie "n")))
        (tr
         (td <%= (1+ i) %>)
         (td <%= (setq x a a b b (+ x b)) %>))))
    %>))))
