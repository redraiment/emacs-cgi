#!/usr/local/bin/elisp

(require 'cgi)

(cgi/remove-session "name")
(cgi/redirect "index.el")
