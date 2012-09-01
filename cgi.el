;;; cgi.el -- using Emacs Lisp for CGI scripting
;;;
;;; Author: Zhang, Zepeng (Joe) <redraiment@gmail.com>
;;; version: 0.2
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

;; Cookie

(defvar cgi/cookies (make-hash-table :test #'equal)
  "HTTP cookies map")

(defun cgi/cookie (key &optional value seconds)
  "Get value from http cookie by key when value is ignore.
Set key = value to cookie when value is not nil.
One day for seconds by default."
  (if value
      (puthash
       key
       (cons value (or seconds 31536000))
       cgi/cookies)
    (let ((value (gethash key cgi/cookies)))
      (car value))))

(defun cgi/remove-cookie (key)
  "Expire the cookie by key."
  (cgi/cookie key "" -100))

; Parse Cookies
(let ((s (getenv "HTTP_COOKIE")))
  (when (and (stringp s) (< 0 (length s)))
    (dolist (c (split-string s ";[[:space:]]*"))
      (let ((plist (xml/string-to-attr c)))
        (cgi/cookie (car plist) (cdr plist))))))

;; HTTP

(defvar cgi/http-host (getenv "HTTP_HOST")
  "HTTP Hostname")

(defun cgi/http-GMT (&optional time)
  "Returns formatted date-time on GMT.
TIME is specified as (HIGH LOW USEC PSEC), as returned by
`current-time' or `file-attributes'.  The obsolete form (HIGH . LOW)
is also still accepted."
  (let ((system-time-locale "C"))
    (format-time-string
     "%a, %d %b %Y %H:%M:%S GMT"
     (or time (current-time)) t)))

(defun cgi/http-set-cookie ()
  (let ((content ""))
    (maphash
     (lambda (key value)
       (setq content (concat content
         (format
          "Set-Cookie: %s=%s; path=/; expires=%s\n"
          key (car value)
          (cgi/http-GMT
           (time-add (current-time)
                     (seconds-to-time (cdr value))))))))
     cgi/cookies)
    content))

(defun cgi/http-head ()
  "HTML always"
  (concat
   "Content-Type: text/html; charset=UTF-8\n"
   (cgi/http-set-cookie)
   "\n"))

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

;; Session
; TODO

(provide 'cgi)
