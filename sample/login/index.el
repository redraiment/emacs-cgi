#!/usr/local/bin/elisp

(require 'cgi)

(html
 (head
  (title "Hello Session")
  (meta (http-equiv . "Content-Type")
        (content . "text/html; charset=UTF-8")))
 (body
  (h1 "Welcome to Emacs Lisp CGI")
   <%
   (if (cgi/session "name")
       (form (method . post) (action . "logout.el")
        (div
         "Hello " <%= (cgi/session "name") %>
         (button (type . submit) logout)))
     (form (method . post) (action . "login.el")
      (div
       "My name is: "
       (input (type . text)
              (name . name))
       (button (type . submit) Submit))))
   %>)))
