#!/usr/local/bin/elisp

(require 'cgi)

(html
 (head
  (title "In Forward-1"))
 (body
  <% (cgi/forward "forward-2.el") %>))
