#!/usr/local/bin/elisp

(require 'cgi)

(if (cgi/param "name")
    (cgi/session "name" (cgi/param "name")))
(cgi/redirect "index.el")
