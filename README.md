emacs-cgi
=========

Using Emacs Lisp for CGI scripting

A simple library for the Common Gateway Interface for Emacs,
allowing you to service requests for non static web pages in elisp.

A sample of elisp CGI script is like below, place it in your
web server's CGI directory (typically called something like
/var/www/cgi-bin/):

```elisp
#!/usr/local/bin/emacs --script

(princ "Content-Type: text/html; charset=UTF-8\r\n\r\n")
(princ "<h1>Hello Emacs Lisp</h1>\n")
```

Usage: a fabonacci sample.

```elisp
#!/usr/local/bin/emacs --script

(require 'cgi)

(html
 (head
  (title "Fabonacci 1 -&gt; n")
  (meta (http-equiv . "Content-Type")
        (content . "text/html; charset=UTF-8")))
 (body
  (form
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
      (dotimes (i (string-to-int
                   (or (cgi-get "n") "15")))
        (tr
         (td <%= (1+ i) %>)
         (td <%= (setq x a a b b (+ x b)) %>))))
    %>))))
```
