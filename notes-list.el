;;; notes-list.el --- Notes list -*- lexical-binding: t -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.

;; Maintainer: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; URL: https://github.com/rougier/notes-list
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Notes list collects notes in user-defined directories and populate a buffer with a two-lines summary for each note. To do so, notes are parsed such as to extract title, icon, date, summary and tags. A typical org note header is thus
;;
;;  #+TITLE:    Emacs hacking
;;  #+DATE:     2023-03-17
;;  #+FILETAGS: HACK EMACS CODE
;;  #+SUMMARY:  Notes about emacs hacking ideas
;;  #+ICON:     bootstrap/journal-code
;;
;; Icon are built using the svg-lib library and syntax is
;; "collection/name" where collection is one one "simple",
;; "bootstrap", "material" or "octicons". For available icons, please
;; refere to the svg-lib documentation.

;;; News
;;
;;  Version 0.1.0
;;  Initial version
;;
;;; Code:
(require 'stripes)
(require 'svg-lib)
(require 'svg-tag-mode)
(require 'cl-lib)
(require 'text-property-search)

(defgroup notes-list nil
  "Note list"
  :group 'convenience)

(defcustom notes-list-directories '("~/Documents/Denotes/")
  "List of directories where to search notes"
  :type '(repeat directory)
  :group 'notes-list)

(defcustom notes-list-sort-function #'notes-list-compare-modification-time
  "Criterion for sorting notes"
  :type '(choice (const :tag "Title"             notes-list-compare-title)
                 (const :tag "Access time"       notes-list-compare-access-time)
                 (const :tag "Creation time"     notes-list-compare-creation-time)
                 (const :tag "Modification time" notes-list-compare-modification-time))
  :group 'notes-list)

(defcustom notes-list-sort-order #'descending
  "Notes sorting order"
  :type '(choice (const :tag "Ascending"  ascending)
                 (const :tag "Descending" descending))
  :group 'notes-list)

(defcustom notes-list-date-display 'modification
  "Which date to display in the list"
  :type '(choice (const :tag "Access time"       access)
                 (const :tag "Creation time"     creation)
                 (const :tag "Modification time" modification))
  :group 'notes-list)

(defcustom notes-list-display-icons t
  "Display icon on (left)"
  :type 'boolean
  :group 'notes-list)

(defcustom notes-list-display-tags t
  "Display tags (top right)"
  :type 'boolean
  :group 'notes-list)

(defcustom notes-list-display-date t
  "Display date (bottom right)"
  :type 'boolean
  :group 'notes-list)

(defface notes-list-face-title
  '((t (:inherit (nano-salient nano-strong))))
  "Face for notes title"
  :group 'notes-list)

(defface notes-list-face-tags
  '((t (:inherit nano-salient)))
  "Face for notes tags"
  :group 'notes-list)

(defface notes-list-face-summary
  '((t (:inherit nano-default)))
  "Face for notes summary"
  :group 'notes-list)

(defface notes-list-face-time
  '((t (:inherit nano-faded)))
  "Face for notes time"
  :group 'notes-list)

(defface notes-list-face-stripe
  `((t (:inherit highlight)))
  "Face to use for alternating note style in list.")

(defface notes-list-face-highlight
  `((t (:inherit nano-subtle)))
  "Face to use for selected note style in list.")

(defvar notes-list--icons nil
  "Icons cache")

(defvar notes-list-collect-notes-function #'notes-list-collect-org-notes
  "Function to used to build list of notes to display. Customize
this combined with `notes-list-open-function' to adapt notes-list
to your favourite notes solution.")

(defvar notes-list-open-function #'find-file
  "Function used to open notes. Customize this combined with
`notes-list-collect-notes-function' to adapt notes-list to your
favourite notes solution.")

(defun notes-list--make-icon (icon)
  "Format given ICON description as a two lines square image.

ICON can be either a filename or a string 'collection/icon-name'
according to svg-lib. The resulting image fits exacly the character
grid such that the squareness of the image is not guaranteed and
depends on the character pixel size. The returned image is split in
two parts (top . bottom)"

  (unless (assoc icon notes-list--icons)
    (let* ((image (if (file-regular-p icon)
                      (create-image icon)
                    (let ((collection (nth 0 (split-string icon "/")))
                          (name (nth 1 (split-string icon "/"))))
                      (svg-lib-icon name nil :collection collection
                                             :stroke 0
                                             :scale .75
                                             :padding 0))))
           (img-width (car (image-size image t)))
           (img-height (cdr (image-size image t)))
           (ch (frame-char-height))
           (cw (frame-char-width))
           (icon-height (* 2 ch))
           (char-width  (+ 1 (truncate (/ (* 2 ch) cw))))
           (icon-width  (* char-width cw))
           ;; (scale (if (< img-height img-width)
           ;;           (/ (float icon-height) (float img-height))
           ;;         (/ (float icon-width) (float img-width))))
           (scale (/ (float icon-height) (float img-height)))

           (scaled-width (truncate (* scale img-width)))
           (scaled-height (truncate (* scale img-height)))
           (icon-true-width (truncate (* img-width scale)))
           (margin (max 0 (- icon-width icon-true-width)))
           (icon-width (+ icon-width (% margin 2)))
           (margin (- margin (% margin 2)))
           (thumbnail (cons (car image) (cl-copy-list (cdr image)))))
    (plist-put (cdr thumbnail) :height scaled-height)
    (plist-put (cdr thumbnail) :width  scaled-width)
    (plist-put (cdr thumbnail) :margin (cons (/ margin 2) 0))
    (plist-put (cdr thumbnail) :ascent 80)

    (push (cons icon
                (cons (propertize (make-string char-width ?-)
                                  'display (list (list 'slice 0  0 icon-width ch) thumbnail)
                                  'line-height t)
                      (propertize (make-string char-width ?-)
                                  'display (list (list 'slice 0  ch icon-width ch) thumbnail)
                                  'line-height t)))
          notes-list--icons)))
    (cdr (assoc icon notes-list--icons)))

(defvar notes-list--svg-tags nil
  "SVG tags cache")

(defun notes-list--make-tag (tag)

  (let ((svg-tag (if (string-equal tag "INBOX")
                     (svg-tag-make tag :face 'notes-list-face-tags :inverse t)
                   (svg-tag-make tag :face 'default))))
  (propertize (concat tag " ") 'display svg-tag)))

(defun notes-list-format-tags (tags)
  "Transform a list of tags into a SVG tags string"

  (mapconcat (lambda (tag)
               (unless (assoc tag notes-list--svg-tags)
                 (setq notes-list--svg-tags
                       (add-to-list 'notes-list--svg-tags
                                    (cons tag (notes-list--make-tag tag)))))
               (cdr (assoc tag notes-list--svg-tags)))
             tags " "))

(defun notes-list-format-title (title)
  (propertize title 'face 'notes-list-face-title))

(defun notes-list-format-time (time)
  (let ((time (format-time-string "%B %d, %Y" time)))
    (propertize time 'face 'notes-list-face-time)))

(defun notes-list-format-summary (summary)
  (propertize summary 'face 'notes-list-face-summary))


(defun notes-list-insert-formatted (note)
  "This function format a note. Result is a two-lines string with
title at top left, time at top-right, summary at bottom left and
tags at bottom right. If TITLE or SUMMARY is too long, it is
truncated."

  (let* ((window (get-buffer-window (notes-list-buffer)))
         (width (- (window-width window) 1))
         (icon (or (cdr (assoc "ICON" note)) "note-outline"))
         (icon (notes-list--make-icon icon))
         (filename (cdr (assoc "FILENAME" note)))

         (tags (or (cdr (assoc "TAGS" note)) ""))
         (tags (notes-list-format-tags tags))
         (tags (if notes-list-display-tags
                   tags
                 ""))
         (time (or
                (cond ((eq notes-list-date-display 'creation)
                       (cdr (assoc "TIME-CREATION" note)))
                      ((eq notes-list-date-display 'access)
                       (cdr (assoc "TIME-ACCESS" note)))
                      (t
                       (cdr (assoc "TIME-MODIFICATION" note))))
                   ""))
         (time (notes-list-format-time time))
         (time (if notes-list-display-date
                   time
                 ""))

         (title (or (cdr (assoc "TITLE" note)) ""))
         (title (notes-list-format-title title))
         (title (if notes-list-display-icons
                    (concat (car icon) " " title)
                  title))
         (title (concat (propertize " " 'display '(raise 0.5)) title))
         (title (truncate-string-to-width
                    title
                    (- width (length time) 1) nil nil "…"))

         (summary (or (cdr (assoc "SUMMARY" note)) ""))
         (summary (notes-list-format-summary summary))
         (summary (if notes-list-display-icons
                      (concat (cdr icon) " " summary)
                    summary))
         (summary (concat (propertize " " 'display '(raise -0.5))
                              summary))
         (summary (truncate-string-to-width summary
                             (- width (length tags) 2) nil nil "…")))

    (let ((start (point)))
      (notes-list-insert-line-with-filler title time)
      (notes-list-insert-line-with-filler summary tags)
      (put-text-property start (point)
                         'filename filename))))

(defun notes-list--pixel-width-between-points (point1 point2)
  "Calculate pixel width of buffer contents between POINT1 and POINT2."
  ;; Recenter to make as likely as possible for point to be visible in the
  ;; current window. This is required for posn-at-point not to return nil.
  (recenter)
  ;; Force redisplay to make sure that posn-at-point is accurate (and does not
  ;; return nil)
  (redisplay t)
  (save-excursion
    (let* ((posn1 (progn
                   (goto-char point1)
                   (posn-at-point)))
           (posn2 (progn
                    (goto-char point2)
                    (posn-at-point)))
           (ydelta (- (cdaddr posn2)
                      (cdaddr posn1)))
           (window-width (window-width (selected-window) t))
           (line-height (line-pixel-height))
           (lines 1))
      (when (> ydelta 0)
        (setq lines (+ (/ ydelta line-height) 1)))

      (let ((width (* lines window-width)))
        (setq width (- width (caaddr posn1) (- window-width (caaddr posn2))))))))

(defun notes-list-insert-line-with-filler (item1 item2)
  "Insert ITEM1 and ITEM2, filling the space between them to guarantee a line width of `window-width'."
  (let ((start-of-line (point)))
    (insert item1)
    (let ((item2-start (point))
          (item2-end)
          (item1-pixel-width (notes-list--pixel-width-between-points start-of-line (point)))
          (item2-pixel-width)
          (window-width (window-text-width (selected-window) t))
          (filler-width))
      (insert item2)
      (setq item2-end (point))
      (setq item2-pixel-width (notes-list--pixel-width-between-points item2-start item2-end))
      (goto-char item2-start)
      (setq filler-width (- window-width item1-pixel-width item2-pixel-width))
      (insert
       (propertize " " 'display
                   `(space :width (,filler-width))))

      ;; Go to the end of whatever has been inserted
      (goto-char (+ item2-end (- (point) item2-start))))))


(defun notes-list-parse-org-note (filename)
  "Parse an org file and extract title, date, summary and tags that
need to be defined at top level as keywords."

  (let ((keep (find-buffer-visiting filename)))
    (with-current-buffer (find-file-noselect filename)
      (let* ((attributes (file-attributes filename))
             (size (file-attribute-size attributes))
             (access-time (file-attribute-access-time attributes))
             (modification-time (file-attribute-modification-time attributes))
             (info (org-collect-keywords '("TITLE"
                                           "ICON"
                                           "DATE"
                                           "SUMMARY"
                                           "FILETAGS")))
           (title (cadr (assoc "TITLE" info)))
           (icon (cadr (assoc "ICON" info)))
           (date (cadr (assoc "DATE" info)))
           (time (parse-time-string date))
           (time (let ((n 0))
                   (mapcar (lambda (x)
                             (if (< (setq n (1+ n)) 7) (or x 0) x)) time)))
           (time (encode-time time))
           (summary (cadr (assoc "SUMMARY" info)))
           (tags (cadr (assoc "FILETAGS" info)))
           (tags (split-string tags)))
        (unless keep (kill-buffer))
        (list (cons "FILENAME" filename)
              (cons "TITLE" title)
              (cons "ICON" icon)
              (cons "TIME-CREATION" time)
              (cons "TIME-MODIFICATION" modification-time)
              (cons "TIME-ACCESS" access-time)
              (cons "SUMMARY" summary)
              (cons "TAGS" tags))))))


(defun notes-list-compare-creation-time (note-1 note-2)
  (time-less-p (cdr (assoc "TIME-CREATION" note-1))
               (cdr (assoc "TIME-CREATION" note-2))))

(defun notes-list-compare-access-time (note-1 note-2)
  (time-less-p (cdr (assoc "TIME-ACCESS" note-1))
               (cdr (assoc "TIME-ACCESS" note-2))))

(defun notes-list-compare-modification-time (note-1 note-2)
  (time-less-p (cdr (assoc "TIME-MODIFICATION" note-1))
               (cdr (assoc "TIME-MODIFICATION" note-2))))

(defun notes-list-compare-title (note-1 note-2)
  (string-lessp (cdr (assoc "TITLE" note-1))
                (cdr (assoc "TITLE" note-2))))

(defun notes-list-note-p (filename)
  "Return t if FILENAME names a note file."

  (file-regular-p filename))

(defvar notes-list--notes nil
  "List of collected notes")

(defun notes-list-collect-notes ()
  "Collect notes from note directories"
  (let ((recentf-list-saved recentf-list)
        (notes (funcall notes-list-collect-notes-function)))
    (setq notes-list--notes notes)
    (setq recentf-list recentf-list-saved))
  notes-list--notes)


(defun notes-list-collect-org-notes ()
  (let ((notes nil))
    (dolist (directory notes-list-directories)
      (dolist (filename (directory-files directory t ".*\\.org"))
        (when (notes-list-note-p filename)
          (let ((note (notes-list-parse-org-note filename)))
            (setq notes (add-to-list 'notes note))))))
    notes))

(defun notes-list-quit ()
  (interactive)
  (kill-buffer))

(defun notes-list-next-note ()
  (interactive)
  (forward-line 1)
  (when (eq (point) (point-max))
    (goto-char (point-min))))

(defun notes-list-prev-note ()
  (interactive)
  (if (eq (point) (point-min))
      (goto-char (- (point-max) 1))
    (forward-line -1)))


(defun notes-list-open (filename)
  (funcall notes-list-open-function filename))

 (defun notes-list-open-note ()
   (interactive)
   (let ((filename (get-text-property (point) 'filename)))
    (notes-list-open filename)))


(defun notes-list-open-note-other-window ()
  (interactive)
  (let ((filename (get-text-property (point) 'filename)))
    (other-window 1)
    (notes-list-open filename)))

(defun notes-list-show-note-other-window ()
  (interactive)
  (let ((filename (get-text-property (point) 'filename)))
    (with-selected-window (next-window)
      (find-file filename))))

(defvar notes-list--buffer-width nil
  "Notes list buffer width")

(defun notes-list--resize-hook (frame)
  "Refresh notes list if necessary"
  (when-let* ((window (get-buffer-window (notes-list-buffer))))
    (let ((window-width (window-text-width window t)))
      (unless (eq window-width notes-list--buffer-width)
        (notes-list-refresh))
      (setq notes-list--buffer-width window-width))))

(defun notes-list-reload ()
  "Rebuild the note list"

  (interactive)
  (notes-list-collect-notes)
  (notes-list-refresh))

(defun notes-list-reverse-sort-order ()
  "Reverse sort order (ascending <-> descending)"

  (interactive)
  (if (eq notes-list-sort-order 'ascending)
      (setq notes-list-sort-order 'descending)
    (setq notes-list-sort-order 'ascending))
  (notes-list-refresh))


(defun notes-list-refresh ()
  "Rebuild the note list if necessary (no reload)"

  (interactive)

  (when-let* ((window (get-buffer-window (notes-list-buffer))))
    (with-selected-window window
      (let* ((notes (cl-copy-list notes-list--notes))
             (notes (sort notes notes-list-sort-function))
             (notes (if (eq notes-list-sort-order #'ascending)
                        notes
                      (reverse notes))))
        (with-current-buffer (notes-list-buffer)
          (let ((filename (get-text-property (point) 'filename)))
            (beginning-of-line)
            (let ((line (count-lines 1 (point)))
                  (inhibit-read-only t))
              (erase-buffer)
              (dolist (note notes)
                (notes-list-insert-formatted note)
                (insert "\n"))
              (insert "\n")
              (goto-char (point-min))
              (let ((match (text-property-search-forward 'filename filename t)))
                (if match
                    (goto-char (prop-match-beginning match))
                  (forward-line line)))
              (beginning-of-line))))))))

(defun notes-list-toggle-icons ()
  "Toggle icons display"

  (interactive)
  (if notes-list-display-icons
      (setq notes-list-display-icons nil)
    (setq notes-list-display-icons t))
  (notes-list-refresh))

(defun notes-list-toggle-date ()
  "Toggle date display"

  (interactive)
  (if notes-list-display-date
      (setq notes-list-display-date nil)
    (setq notes-list-display-date t))
  (notes-list-refresh))

(defun notes-list-toggle-tags ()
  "Toggle tags display"

  (interactive)
  (if notes-list-display-tags
      (setq notes-list-display-tags nil)
    (setq notes-list-display-tags t))
  (notes-list-refresh))

(defun notes-list-buffer ()
  "Return the notes list buffer"

  (get-buffer-create "*notes-list*"))

(define-minor-mode notes-list-mode
  "A minor mode for browsing note list"

  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "d") #'notes-list-toggle-date)
            (define-key map (kbd "i") #'notes-list-toggle-icons)
            (define-key map (kbd "t") #'notes-list-toggle-tags)
            (define-key map (kbd "r") #'notes-list-reload)
            (define-key map (kbd "g") #'notes-list-refresh)
            (define-key map (kbd "q") #'notes-list-quit)
            (define-key map (kbd "s") #'notes-list-reverse-sort-order)
            (define-key map (kbd "SPC") #'notes-list-show-note-other-window)
            (define-key map (kbd "<tab>") #'notes-list-open-note-other-window)
            (define-key map (kbd "<RET>") #'notes-list-open-note)
            (define-key map (kbd "<left") nil)
            (define-key map (kbd "<right") nil)
            (define-key map (kbd "<up>") #'notes-list-prev-note)
            (define-key map (kbd "<down>") #'notes-list-next-note)
            map)
  (when notes-list-mode
    (setq stripes-unit 1)
    (stripes-mode t)
    (setq hl-line-overlay-priority 100)
    (hl-line-mode t)
    (face-remap-set-base 'stripes :inherit 'notes-list-face-stripe)
    (face-remap-add-relative 'hl-line :inherit 'notes-list-face-highlight)
    (setq-local cursor-type nil)
    (read-only-mode t)
    (add-hook 'window-size-change-functions #'notes-list--resize-hook)))

;;;###autoload
(defun notes-list ()
  "Display note list in current buffer"

  (interactive)
  (switch-to-buffer (notes-list-buffer))
  (notes-list-reload)
  (notes-list-mode 1))

(provide 'notes-list)
;;; notes-list.el ends here
