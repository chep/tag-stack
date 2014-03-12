;;tag-stack.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;; Copyright Cédric Chépied 2014

(defvar tag-stack-stack nil
  "The Stack")

(defvar tag-stack-stack-pointer 0
  "The Stack")


(defun tag-stack-get-buffer (tag)
  (nth 3 tag))

(defun tag-stack-get-position (tag)
  (nth 2 tag))

(defun tag-stack-get-args (tag)
  (nth 1 tag))

(defun tag-stack-get-name (tag)
  (nth 0 tag))



(defun tag-stack-begin ()
  "Create a new stack for tags"
  (interactive)
  (setq tag-stack-stack (list (list "Begining" nil (point) (current-buffer)))
        tag-stack-stack-pointer 0)
  (tag-stack-update-buffer)
)

(defun tag-stack-find-tag (tagname &optional next-p)
  (interactive (find-tag-interactive "Find tag: "))
  (let* ((current-buf (current-buffer))
         (current-pos (point))
         (buf (find-tag-noselect tagname next-p nil))
         (pos (with-current-buffer buf (point))))
    (when (and buf pos)
      (unless tag-stack-stack
        (tag-stack-begin))
      ;; remove stack after pointer and append new tag
      (let* ((st (butlast tag-stack-stack
                         (- (safe-length tag-stack-stack)
                            tag-stack-stack-pointer)))
             (elt (nth tag-stack-stack-pointer tag-stack-stack)))
        (setq tag-stack-stack (append st
                                      (list (list (tag-stack-get-name elt)
                                                  (tag-stack-get-args elt)
                                                  current-pos
                                                  current-buf))
                                      (list (list tagname
                                                  (tag-stack-get-tag-args current-buf
                                                                          current-pos)
                                                  pos
                                                  buf)))))
      ;; increment pointer
      (setq tag-stack-stack-pointer (1+ tag-stack-stack-pointer))

      ;;go to tag
      (switch-to-buffer buf)
      (goto-char pos)
      (tag-stack-update-buffer))))


(defun tag-stack-get-tag-args (buf pos)
  "return the list of arguments for the tag"
  ;;arg-current => arguments of the current function
  ;;arg-current-real => arguments given to the current function
  ;;arg-next => argument given to the next function
  (save-excursion
    (switch-to-buffer buf)
    (goto-char pos)
    (let* ((arg-current-real (tag-stack-get-args (nth tag-stack-stack-pointer
                                                      tag-stack-stack)))
           (arg-next (tag-stack-get-args-at-point))
           (arg-current (with-current-buffer buf (semantic-get-local-arguments)))
           (arg-list nil))
      (when arg-next
        (dolist (one-arg arg-next) ;;for all args
          (let ((arg "")) ;;arg will be the complete argument
            (dolist (sexp-arg one-arg) ;; for all parts of an arg
              (let ((sexp-final sexp-arg)
                    (index 0))
                (when (and arg-current arg-current-real)
                  (dolist (c arg-current)
                    (when (string-match-p (concat "^[^[:alnum:]]*" (nth 0 c) "[^[:alnum:]]*$")
                                          sexp-final)
                      (setq sexp-final (replace-regexp-in-string (nth 0 c)
                                                                 (nth index arg-current-real)
                                                                 sexp-final)))
                    (setq index (1+ index))))
                (setq arg (concat arg sexp-final))))
            (setq arg-list (append arg-list (list arg))))))
      arg-list)))

(defun tag-stack-get-args-at-point ()
  "return arguments given to the function at point
This is a list of sexp list"
  (let ((args nil))
    (save-excursion

        (forward-sexp) ;; go to first parenthesis
        (forward-char) ;; got o first argument

        (while (not (char-equal (char-after) ?\) ))
          (skip-chars-forward ",[:blank:]\n")
          (let ((beg (point))
                (c (char-after))
                (arg nil))
            (while (not (or (char-equal c ?,)
                            (char-equal c ?\) )))
              (forward-sexp)
              (setq arg (append arg
                                (list (buffer-substring-no-properties beg (point)))))
              (setq beg (point)
                    c (char-after)))
            (setq args (append args
                               (list arg))))))
    args))


(defun tag-stack-update-buffer ()
  (let ((buf (get-buffer-create "*tag-stack*"))
        (current 0))
    (with-current-buffer buf
      (progn (setq buffer-read-only nil)
             (erase-buffer)
             (dolist (tag tag-stack-stack)
               (progn (if (= current tag-stack-stack-pointer)
                        (insert "> ")
                        (insert "  "))
                      (insert (concat (tag-stack-get-name tag)
                                      " ("))
                      (when (tag-stack-get-args tag)
                        (progn (dolist (arg (tag-stack-get-args tag))
                                 (insert (concat arg ", ")))
                               (delete-backward-char 2)))
                      (insert (concat ") "
                                      (buffer-file-name (tag-stack-get-buffer tag))
                                      " line "
                                      (with-current-buffer (tag-stack-get-buffer tag)
                                        (number-to-string (line-number-at-pos
                                                           (tag-stack-get-position tag))))))
                      (newline))
               (setq current (1+ current)))
             (setq buffer-read-only t)))))


(defun tag-stack-up()
  (interactive)
  (when (and tag-stack-stack
             (> tag-stack-stack-pointer 0))
    (progn (setq tag-stack-stack-pointer (1- tag-stack-stack-pointer))
           (let ((tag (nth tag-stack-stack-pointer
                            tag-stack-stack)))
           (switch-to-buffer (tag-stack-get-buffer tag))
           (goto-char (tag-stack-get-position tag)))
           (tag-stack-update-buffer))))


(defun tag-stack-down()
  (interactive)
  (when (and tag-stack-stack
             (< tag-stack-stack-pointer (1- (length tag-stack-stack))))
    (progn (setq tag-stack-stack-pointer (1+ tag-stack-stack-pointer))
           (let ((tag (nth tag-stack-stack-pointer
                            tag-stack-stack)))
           (switch-to-buffer (tag-stack-get-buffer tag))
           (goto-char (tag-stack-get-position tag)))
           (tag-stack-update-buffer))))




