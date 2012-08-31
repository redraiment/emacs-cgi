;;; cgi.el -- using Emacs Lisp for CGI scripting
;;;
;;; Author: Zhang, Zepeng (Joe) <redraiment@gmail.com>
;;; version: 0.1
;;; Time-stamp: <2012-08-31 CST>
;;; Copyright: (C) 2012 Zhang, Zepeng

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(require 'xml-generator)

;; HTTP

(defun cgi/http-head ()
  "HTML always"
  "Content-Type: text/html; charset=UTF-8\n\n")

;; DOM

(defconst cgi/html401-dtd
  (concat
   "<!DOCTYPE HTML PUBLIC\n"
   " \"-//W3C//DTD HTML 4.01//EN\"\n"
   " \"http://www.w3.org/TR/html4/strict.dtd\">\n")
  "HTML 4.01 Strict DTD")

(defconst cgi/xhtml10-dtd
  (concat
   "<!DOCTYPE HTML PUBLIC\n"
   " \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n"
   " \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n")
  "XHTML 1.0 Strict DTD")

(defmacro html (&rest args)
  "With HTML 4.01 DTD"
  `(princ
    (concat
     ,(cgi/http-head)
     ,cgi/html401-dtd
     (xml/gen '(html ,@args)))))

(defmacro xhtml (&rest args)
  "With XHTML 1.0 DTD"
  `(princ
    (concat
     ,(cgi/http-head)
     ,cgi/xhtml10-dtd
     (xml/gen '(html ,@args)))))

(defmacro cgi/register-tag (&rest tags)
  `(progn
     ,@(mapcar
        (lambda (tag)
          `(defmacro ,(intern (symbol-name tag)) (&rest args)
             `(princ (xml/gen (cons ',',tag ',args)))))
        tags)))

; register common HTML/4.1 tags
(cgi/register-tag
 head title meta body
 h1 h2 h3 h4 h5 h6 hr
 table thead tbody tfoot tr th td
 ol ul li
 a blockquote p br code div span font pre
 form button input select option optgroup textarea)

;; URL encode & decode

(defun cgi/decode (s)
  "Substrings are plus-decoded and then URI-decoded."
  (replace-regexp-in-string
   "%[[:xdigit:]]\\{2\\}"
   (lambda (hex)
     (char-to-string
      (string-to-number
       (substring hex 1)
       16)))
   (replace-regexp-in-string "\\+" " " s)))

(defun cgi/encode (s)
  "URI-encode first, then plus-encode."
  (replace-regexp-in-string
   " " "+"
   (replace-regexp-in-string
    "[^-A-Za-z0-9!*().~' ]"
    (lambda (c)
      (format "%%%02X" (string-to-char c)))
    s)))

(defun cgi/param-decode (url)
  "Parse \"name=Zhang%2C+Joe&age=23\"
into ((name . \"Zhang, Joe\") (age . 23))"
  (if (and (stringp url) (< 0 (length url)))
      (mapcar
       (lambda (param)
         (let ((attr (xml/string-to-attr param)))
           (cons (car attr)
                 (cgi/decode (cdr attr)))))
       (split-string url "&"))))

(defun cgi/param-encode (plist)
  "Parse ((name . \"Zhang, Joe\") (age . 23))
into \"name=Zhang%2C+Joe&age=23\""
  (mapconcat
   (lambda (cell)
     (format "%s=%s"
             (car cell)
             (cgi/encode (cdr cell))))
   plist "&"))

;; Request Parameters

(defconst cgi/request-method
  (getenv "REQUEST_METHOD")
  "CGI Request Method")

(defvar cgi/parameters
  (cgi/param-decode
   (cond
    ((string= "GET" cgi/request-method)
     (getenv "QUERY_STRING"))
    ((string= "POST" cgi/request-method)
     (with-temp-buffer
       (dotimes (_ (string-to-number
                    (getenv "CONTENT_LENGTH")))
         (insert (read-event)))
       (buffer-string)))
    (t "")))
  "Parameter list from GET or POST request.")

(defun cgi/param (key)
  "Returns value string of the GET or POST variable."
  (cdr (assoc key cgi/parameters)))

(defun cgi/params (key)
  "Returns value string list of the GET or POST variables."
  (delq nil
    (mapcar
     (lambda (cell)
       (if (equal key (car cell))
           (cdr cell)))
     cgi/parameters)))

;; Cookie
; TODO

;; Session
; TODO

(provide 'cgi)
