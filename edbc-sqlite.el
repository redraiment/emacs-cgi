;;; edbc-sqlite -- Emacs Database Connectivity for sqlite
;;;
;;; Author: Zhang, Zepeng (Joe) <redraiment@gmail.com>
;;; version: 0.1
;;; Time-stamp: <2012-09-04 CST>
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

;;; Usage
;; Assume that there is a Sqlite DB named `users.db' and it has one
;; table named `users', which contains id and name two fields.

;; (sqlite/with-connect "users.db"
;;   (let ((id 1)
;;         (name "Joe")
;;         (nickname "redraiment"))
;;     ; purge table
;;     (db/delete from users)
;;     ; Equals run insert into users (name) values ('Joe') on sqlite
;;     (db/insert into users (id, name) values (:id, :name))
;;     ; Equals run (db/update users set name = 'redraiment' where name = 'Joe' on sqlite
;;     (db/update users set name = : nickname where id = :(identity id))
;;     ; Returns ((("id" . "1") ("name" . "redraiment")))
;;     (db/select * from users)))

(defconst db/process-name "sqlite"
  "The name of sqlite process in Emacs.")

(defconst db/buffer-name "*edbc-sqlite-output-buffer*"
  "The buffer name for sqlite process output.")

(defvar db/command-name "sqlite3"
  "The command for sqlite.")

(defvar db/command-args '("-batch" "-line")
  "The command arguments for sqlite.")

(defun sqlite/sexp-to-command (sexp)
  "Convert a sexp to one sqlite command string.
It will append ;\\n end of line."
  (flet ((sexp-convert (sql)
          (cond
           ((eq sql ':)
            (setf eval-content? t)
            "")
           ((keywordp sql)
            (sexp-convert
             (symbol-value
              (intern (substring (symbol-name sql) 1)))))
           (eval-content?
            (setf eval-content? nil)
            (sexp-convert
             (if (symbolp sql)
                 (symbol-value sql)
               (eval sql))))
           ((stringp sql)
            (format
             "'%s'"
             (replace-regexp-in-string "'" "''" sql)))
           ((consp sql)
            (let ((statement (mapconcat #'sexp-convert sql " ")))
              (if (eq (car sql) '\,)
                  statement
                (concat "(" statement ")"))))
           (t (format "%s" sql)))))
    (let ((eval-content? nil))
      (concat (substring (sexp-convert sexp) 1 -1) ";\n"))))

(defmacro sqlite/with-connect (url &rest body)
  "Connect sqlite database by url.
In body, you can use `db/select' to query, `db/insert' to add and
`db/update' to modify. All those three operators can use inside
of `sqlite/with-connect' only. Use can use `:' to refer sexp."
  (let ((var-pid (gensym "pid"))
        (var-result (gensym "result"))
        (var-trim (gensym "trim"))
        (var-parse (gensym "parse")))
    `(let ((,var-pid (start-process db/process-name db/buffer-name
                                    db/command-name ,@db/command-args
                                    ,url)))
       (flet
           ((,var-trim (s)
             ; string trim both
             (replace-regexp-in-string
              "^\s+" ""
              (replace-regexp-in-string
               "\s+$" ""
               s)))
            (,var-parse (s)
             ; one row one paragraph, one field one line.
             (mapcar
              (lambda (paragraph)
                (mapcar
                 (lambda (line)
                   (let ((idx (string-match "=" line)))
                     (if idx
                         (cons
                          (,var-trim (substring line 0 idx))
                          (,var-trim (substring line (1+ idx))))
                       (list (,var-trim line)))))
                 (delq "" (split-string paragraph "\r?\n"))))
              (split-string s "\r?\n\r?\n"))))
         (macrolet
             ; Utilities that used to maintain Sqlite
             ((db/select
               (&rest args)
               `(with-current-buffer (process-buffer ,',var-pid)
                  (erase-buffer)
                  (process-send-string
                   ,',var-pid
                   (sqlite/sexp-to-command '(select ,@args)))
                  (accept-process-output ,',var-pid)
                  (,',var-parse (buffer-substring 1 (buffer-size)))))
              (db/update
               (&rest args)
               `(process-send-string
                 ,',var-pid
                 (sqlite/sexp-to-command '(update ,@args))))
              (db/insert
               (&rest args)
               `(process-send-string
                 ,',var-pid
                 (sqlite/sexp-to-command '(insert ,@args))))
              (db/delete
               (&rest args)
               `(process-send-string
                 ,',var-pid
                 (sqlite/sexp-to-command '(delete ,@args)))))
           (setf ,var-result (progn ,@body))
           (process-send-string ,var-pid ".quit\n")))
       ,var-result)))

(provide 'edbc-sqlite)
