;;; xml-generator.el -- convert list to XML string
;;;
;;; Author: Zhang, Zepeng (Joe) <redraiment@gmail.com>
;;; Time-stamp: <2012-08-31 CST>
;;; Copyright: (C) 2012 Zhang, Zepeng (Joe)

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

;; Generate xml using sexps with the function `xml/gen':

;; (xml/gen '(p (class . big)))      => "<p class=\"big\" />"
;; (xml/gen '(p (class . "big") "hi")) => "<p class=\"big\">hi</p>"

;; (xml/gen
;;  '(html
;;    (head
;;     (title "XML")
;;     (meta (http-equiv . Content-Type)
;;           (content . "text/html; charset=UTF-8")))
;;    (body
;;     (h1 "Hello world"))))

;; produces below:

;; <html>
;;  <head>
;;   <title>XML</title>
;;   <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
;;  </head>
;;  <body>
;;   <h1>Hello world</h1>
;;  </body>
;; </html>

;; NOTE: `xml/gen' would not really format the output. It puts
;; all the thing one line. I know another plug-in can do that:
;; `https://github.com/philjackson/xmlgen'

;; Escape
(defconst xml/escapees
  '(("&" . "&amp;")
    ("'" . "&apos;")
    ("\"" . "&quot;")
    ("<" . "&lt;")
    (">" . "&gt;")
    (" " . "&nbsp;"))
  "List of (find . replace) pairs for escaping.")

(defun xml/string-escape (content)
  "Escape CONTENT for inclusion in some XML."
  (when (stringp content)
    (dolist (e xml/escapees)
      (setq content
            (replace-regexp-in-string
             (car e) (cdr e) content))))
  content)

;; Attribute
(defun xml/attrp (plist)
  "Predicate for XML attributs.
Only Accept (key . value)."
  (and (consp plist)
       (atom (car plist))
       (not (listp (cdr plist)))))

(defun xml/attr-to-string (plist)
  "Convert a plist to xml style attributes."
  (if (cdr plist)
      (format "%s=\"%s\"" (car plist) (cdr plist))
    (format "%s" (car plist))))

;; Convert
(defun xml/gen (sexp)
  "Convert a S-EXP to XML string.
'(root (person (name . Joe) (skill elisp)))
=>
<root><person name=\"Joe\"><skill>elisp</skill></person></root>

S-EXP may contain script-let (`<%' and `%>', `<%=' and `%>')
followed by a Lisp expression used as part of the content.
The result of expressions that inside of `<%=' and `%>' will
insert into content, which the output of expressions that inside
of `<%' and `%>' will be inserted.

'(root (name <% (princ \"Joe\") %>) (age <%= 23 %>))
=>
<root><name>Joe</name><age>23</age></root>"
  (let ((tag (format "%s" (car sexp)))
        (attr ())
        (children ())
        (value-content? nil)
        (eval-content? nil))
    (dolist (e (cdr sexp))
      (cond
       ((eq e '%>)
        (setq value-content? nil
              eval-content? nil))
       ((eq e '<%)
        (setq eval-content? t))
       ((eq e '<%=)
        (setq value-content? t))
       (eval-content?
        (push (with-output-to-string (eval e)) children))
       (value-content?
        (push (format "%s" (eval e)) children))
       ((xml/attrp e)
        (push (xml/attr-to-string e) attr))
       ((consp e)
        (push (xml/gen e) children))
       (t
        (push (format "%s" e) children))))
    (format
     "<%s%s%s>"
     tag
     (mapconcat
      (lambda (s) (concat " " s))
      (nreverse attr)
      "")
     (if children
         (format
          ">%s</%s"
          (apply #'concat (nreverse children))
          tag)
       " /"))))

(provide 'xml-generator)
