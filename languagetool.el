;;; languagetool.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'cl-lib)
(require 'eieio)

;;; Custom Variables
(defcustom languagetool-url "https://api.languagetoolplus.com"
  "The LanguageTool API URL.")
(defcustom languagetool-api-username nil
  "The LanguageTool API USERNAME.")

(defcustom languagetool-api-key nil
  "The LanguageTool API KEY.")

(defcustom languagetool-language "auto"
  "The LanguageTool API Language.")

(defcustom languagetool-interval 3
  "The interval for calling the LanguageTool API.")

(defcustom languagetool-log-level 'info
  "")

(defclass languagetool-item ()
  ((line :initarg :line)
   (offset :initarg :offset)
   (length :initarg :length)
   (text :initarg :text)
   (msg :initarg :msg)
   (short-msg :initarg :short-msg)
   (replacements :initarg :replacements)
   (description :initarg :description)
   (type :initarg :type)
   (overlay :initarg :overlay)))

(defclass languagetool-diff ()
  ((buffer :initarg :buffer)
   (old-file :initarg :old-file)
   (now-file :initarg :now-file)
   (delete :initarg :delete)
   (append :initarg :append)
   (text :initarg :text)
   (offset-map :initarg :offset-map)))

;;; Local Variables
(defvar languagetool--block-size 1000)
(defvar languagetool--logger nil)

(defvar-local languagetool--diff nil)
(defvar-local languagetool--items nil)
(defvar-local languagetool--timer nil)
(defvar-local languagetool--block-count 1)
(defvar-local languagetool--incremental-finished-p nil)

;;; Local Functions
(defun languagetool--log (level &rest args)
  (when (featurep 'elog)
    (unless languagetool--logger
      (setq languagetool--logger (elog-logger :name "LanguageTool" :level languagetool-log-level)))
    (pcase level
      ('debug (apply #'elog-debug languagetool--logger args))
      ('info (apply #'elog-info languagetool--logger args))
      ('warning (apply #'elog-warning languagetool--logger args))
      ('error (apply #'elog-error languagetool--logger args))
      ('fatal (apply #'elog-fatal languagetool--logger args)))))

(defun languagetool--ov-p (ov)
  (and ov
       (overlayp ov)
       (equal 'languagetool (overlay-get ov 'source))))

(defun languagetool--at-overlay-p ()
  (let ((ovs (overlays-at (point))))
    (find-if
     #'languagetool--ov-p
     ovs)))

(defun languagetool--delete-overlay (ov)
  (when (languagetool--ov-p ov)
    (delete-overlay ov)))

(defun languagetool--move (move-func end-func)
  (when (languagetool--at-overlay-p)
      (goto-char (funcall move-func (point))))
  (while (not (or (languagetool--at-overlay-p)
                  (funcall end-func)))
    (goto-char (funcall move-func (point)))))

(defun languagetool--ensure-diff ()
  (unless languagetool--diff
    (setq languagetool--diff
          (languagetool-diff
           :buffer (current-buffer)
           :old-file (make-temp-file "languagetool-old-")
           :now-file (make-temp-file "languagetool-now-")))))

(defun languagetool--write-now-file (diff)
  (let ((end-point (if languagetool--incremental-finished-p
                       (point-max)
                       (min (point-max) (* languagetool--block-size languagetool--block-count)))))
   (write-region (point-min) end-point (eieio-oref diff 'now-file) nil 'silent)
   (if (< end-point (point-max))
       (setq languagetool--block-count (1+ languagetool--block-count))
     (setq languagetool--incremental-finished-p t))))

(defun languagetool--copy-now-to-old (diff)
  (copy-file (eieio-oref diff 'now-file) (eieio-oref diff 'old-file) t))

(defun languagetool--remove-item (item)
  (let ((ov (eieio-oref item 'overlay)))
    (delete-overlay ov)
    (setq languagetool--items (remove item languagetool--items))
    (languagetool--log 'info "[Delete] LanguageTool item: %s" item)))

(defun languagetool--parse-diff-line (line)
  (let* ((pair (string-split line "[-+,]" t))
         (begin (string-to-number (nth 0 pair)))
         (count (string-to-number (or (nth 1 pair) "1"))))
    (mapcar (lambda (i) (+ i begin)) (number-sequence 0 (1- count)))))

(defun languagetool--fulfill-diff-offset-map (diff)
  (let ((buffer (eieio-oref diff 'buffer))
        (line-nums (eieio-oref diff 'append))
        (text)
        (original-offset)
        (offset))
    (with-current-buffer buffer
      (save-excursion
        (dolist (num line-nums)
          (goto-line num)
          (setq text (concat text (thing-at-point 'line t)))
          (push (cons (line-beginning-position) (line-end-position)) original-offset))))
    (eieio-oset diff 'text  text)
    (when text
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (while (not (eobp))
          (push (cons (line-beginning-position) (line-end-position)) offset)
          (beginning-of-line 2))))
    (eieio-oset diff 'offset-map  (cl-mapcar #'list line-nums (reverse original-offset) (reverse offset)))))

(defun languagetool--fulfill-diff-with-shell-output (diff lines)
  (let ((delete)
        (append))
    (dolist (line lines)
      (pcase line
        ((pred (string-prefix-p "-")) (setq delete (append delete (languagetool--parse-diff-line line))))
        ((pred (string-prefix-p "+")) (setq append (append append (languagetool--parse-diff-line line))))))
    (eieio-oset diff 'delete delete)
    (eieio-oset diff 'append append)
    (languagetool--fulfill-diff-offset-map diff)))

(defun languagetool--fulfill-diff (diff)
  (languagetool--fulfill-diff-with-shell-output
   diff
   (split-string
    (shell-command-to-string
     (format "diff -U0 \"%s\" \"%s\" | grep ^@@"
             (eieio-oref diff 'old-file)
             (eieio-oref diff 'now-file)))
    "[@ \n]" t)))

(defun languagetool--check (diff)
  (when-let* ((buffer (eieio-oref diff 'buffer))
              (text (eieio-oref diff 'text))
              (data `(("text" . ,text)
                      ("language" . ,languagetool-language))))
    (when languagetool-api-username
      (push `("username" . ,languagetool-api-username) data))
    (when languagetool-api-key
      (push `("apiKey" . ,languagetool-api-key) data))
    (pdd (format "%s/v2/check" languagetool-url)
      :headers '(("Content-Type" . "application/x-www-form-urlencoded")
                 ("accept" . "application/json"))
      :data data
      :done (lambda (res) (languagetool-render diff res))
      :fail (lambda (err) (message "%s" err)))))

(defun languagetool--remove-overlays-in-line (num)
  (save-excursion
    (goto-line num)
    (mapc #'languagetool--delete-overlay (overlays-in (line-beginning-position) (line-end-position)))))

(defun languagetool-render (diff data)
  (let* ((buffer (eieio-oref diff 'buffer))
         (delete (eieio-oref diff 'delete))
         (append (eieio-oref diff 'append))
         (offset-map (eieio-oref diff 'offset-map))
         (matches (append (alist-get 'matches data) nil)))
    (when delete
      (mapc #'languagetool--remove-item
            (cl-remove-if-not (lambda (item) (member (eieio-oref item 'line) delete))
                              languagetool--items))
      (mapc #'languagetool--remove-overlays-in-line delete))
    (setq
     languagetool--items
     (append
      (mapcar (lambda (match) (languagetool--item-build match diff)) matches)
      languagetool--items))))

(defun languagetool--compute-line (match diff)
  (let ((offset-map (eieio-oref diff 'offset-map))
        (offset (alist-get 'offset match)))
    (car (cl-find-if (lambda (map)
                       (and (> offset (car (nth 2 map)))
                            (< offset (cdr (nth 2 map)))))
                     offset-map))))

(defun languagetool--compute-offset (match diff)
  (let* ((offset-map (eieio-oref diff 'offset-map))
         (offset (alist-get 'offset match))
         (map
          (cl-find-if (lambda (map)
                        (and (> offset (car (nth 2 map)))
                             (< offset (cdr (nth 2 map)))))
                      offset-map)))
    (unless map
      (setq map (car (last offset-map))))
    (- (+ offset (car (nth 1 map)) 1) (car (nth 2 map)))))

(defun languagetool--refine (item)
  (let* ((replacements (eieio-oref item 'replacements))
         (msg (eieio-oref item 'msg))
         (begin (eieio-oref item 'offset))
         (end (+ begin (eieio-oref item 'length)))
         (replacement (completing-read (format "[%s] Refine: " msg)
                                       replacements)))
    (languagetool--remove-item item)
    (replace-region-contents begin end (lambda () replacement))))

(defun languagetool--item-build (match diff)
  (languagetool--log 'info "LanguageTool match is : %s" match)
  (let* ((line (languagetool--compute-line match diff))
         (offset (languagetool--compute-offset match diff))
         (length (alist-get 'length match))
         (msg (alist-get 'message match))
         (short-msg (alist-get 'shortMessage match))
         (replacements (mapcar (lambda (alist) (alist-get 'value alist)) (append (alist-get 'replacements match) nil)))
         (type (alist-get 'issueType (alist-get 'rule match)))
         (description (alist-get 'description (alist-get 'rule match)))
         (overlay (make-overlay offset (+ offset length)))
         (map (make-sparse-keymap))
         (item)
         (action (lambda () (interactive) (languagetool--refine item))))
    (overlay-put overlay 'face 'flymake-warning)
    (overlay-put overlay 'help-echo msg)
    (overlay-put overlay 'mouse-face 'highlight)
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'source 'languagetool)
    (setq item (languagetool-item
                :line line
                :offset offset
                :length length
                :msg msg
                :text (buffer-substring offset (+ offset length))
                :short-msg short-msg
                :replacements replacements
                :type type
                :description description
                :overlay overlay))
    (define-key map [mouse-1] action)
    (keymap-set map "RET" action)
    (keymap-set map "C-c n" #'languagetool-next)
    (keymap-set map "C-c p" #'languagetool-previous)
    (overlay-put overlay 'keymap map)
    (languagetool--log 'info "[Add] a new LanguageTool item: %s" item)
    item))

;;; Interactive Functions

(defun languagetool ()
  (interactive)
  (languagetool--ensure-diff)
  (languagetool--write-now-file languagetool--diff)
  (languagetool--fulfill-diff languagetool--diff)
  (languagetool--check languagetool--diff)
  (languagetool--copy-now-to-old languagetool--diff))

(defun languagetool-clear ()
  (interactive)
  (when languagetool--diff
    (setq languagetool--diff nil))
  (when languagetool--items
    (mapc #'languagetool--remove-item languagetool--items))
  (setq languagetool--block-count 1)
  (setq languagetool--incremental-finished-p nil)
  (when (timerp languagetool--timer)
    (cancel-timer languagetool--timer)
    (setq languagetool--timer nil))
  (mapc #'languagetool--delete-overlay
        (car (overlay-lists))))

(defun languagetool-list ()
  (interactive)
  (let ((items languagetool--items))
    (with-current-buffer (get-buffer-create "*languagetool-list*")
      (setq buffer-read-only t)
      (let ((buffer-read-only nil))
       (erase-buffer)
       (dolist (item items)
         (insert
          (eieio-oref item 'text)
          " - "
          (eieio-oref item 'type)
          "\n")))
      (pop-to-buffer (current-buffer)
                     '((display-buffer-in-side-window)
                       (side . right)
                       (slot . 0)
                       (window-width . 0.33)
                       (inhibit-same-window . t))))))

(defun languagetool-next ()
  (interactive)
  (languagetool--move #'next-overlay-change #'eobp))

(defun languagetool-previous ()
  (interactive)
  (languagetool--move #'previous-overlay-change #'bobp))

;;; Define Mode
(define-minor-mode languagetool-mode
  "Minor Mode for LanguageTool."
  :init-value nil
  :lighter " LT"
  (if languagetool-mode
      (let ((buf (current-buffer)))
        (setq languagetool--timer
              (run-with-timer
               nil
               languagetool-interval
               (lambda ()
                 (when (buffer-live-p buf)
                   (with-current-buffer buf
                     (when languagetool-mode
                       (languagetool))))))))
    (languagetool-clear)))

(provide 'languagetool)
;;; languagetool.el ends here
