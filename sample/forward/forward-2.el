#!/usr/local/bin/elisp

(require 'cgi)

(html
 (head
  (title "In Forward-2"))
 (body
  <% (cgi/forward "/cgi-bin/forward-3.el") %>))
