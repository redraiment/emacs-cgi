#!/usr/local/bin/elisp

(require 'cgi)

(cond
 ((cgi/param "name")
  (cgi/session "name" (cgi/param "name")))
 ((cgi/param "logout")
  (cgi/remove-session "name")))

(html
 (head
  (title "Hello Session")
  (meta (http-equiv . "Content-Type")
        (content . "text/html; charset=UTF-8")))
 (body
  (h1 "Welcome to Emacs Lisp CGI")
  (form (method . post)
   <%
   (if (cgi/session "name")
       (div
        "Hello " <%= (cgi/session "name") %>
        (input (type . hidden)
               (name . logout)
               (value . true))
        (button (type . submit) logout))
     (div
      "My name is: "
      (input (type . text)
             (name . name))
      (button (type . submit) Submit)))
   %>)))
