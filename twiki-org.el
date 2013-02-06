;;; twiki-org.el --- major mode for editing twiki documents

;; Copyright (C) 2012 Hugh Brown
;; http://saintaardvarkthecarpeted.com

;; Author: Hugh Brown <aardvark@saintaardvarkthecarpeted.com>
;; Version: 0.8
;; Package-requires: ((org "6.0"))

;; My attempt to base twiki mode on org, rather than outline.
;; Hugh Brown, March 16 2012

;; What works:
;; - M-left/right will promote/demote a heading
;; - C-enter will create a new heading, or a new entry in a list
;; - tab will cycle visibility for headings
;; - I don't seem to have broken tables. :-)


;; Based on the original twiki-mode by Noah S. Friedman.
;; Copyright (C) 2004 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Created: 2004-04-21

;; $Id: twiki-outline.el,v 1.1 2004/04/26 19:15:35 friedman Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;; Commentary:

;; twiki is a free web collaboration platform, http://twiki.org/

;; This mode currently implements outline highlighting and toggling display
;; of html tags, which are sometimes necessary for markup but make outlines
;; harder to read.

;; This mode works best in Emacs 21.  It works with some degradation of
;; functionality in Emacs 20.  I'm not sure about Emacs 19, and I have not
;; tested with XEmacs.

;; TODO: defcustomize, document

;; Code:

(require 'outline)

(defgroup twiki-org nil
  "Major mode for editing twiki documents"
  :group org)

(defvar twiki-org-regexp "---\\(\\++\\)\\(?:!!\\)? ")

(defvar twiki-org-html-tag-face 'twiki-org-html-tag-face
  "Font Lock mode face used to highlight HTML tags in twiki-org mode.")

(defface twiki-org-html-tag-face
    '((t :inherit font-lock-string-face))
    "Font Lock mode face used to highlight HTML tags in twiki-org mode."
    :group 'twiki-org)

(defvar twiki-org-hide-html-tags t)

(defvar twiki-org-font-lock-keywords
  '(("---\\++\\(!!\\)? .*"
     0 (twiki-org-choose-face) nil t)
    ("<[^\>]*>"
     0 '(face           twiki-org-html-tag-face
         invisible      twiki-org-html-tag
         front-sticky   nil
         rear-nonsticky t)
     t t)))

(defvar twiki-org-faces
  [font-lock-warning-face
   font-lock-function-name-face
   font-lock-variable-name-face
   font-lock-keyword-face
   font-lock-builtin-face
   font-lock-comment-face
   font-lock-constant-face
   font-lock-type-face
   font-lock-string-face
   font-lock-doc-face
   font-lock-preprocessor-face])

(defun twiki-org-level ()
  "Calculate level of heading we're on/in.

FIXME: Not sure we need the direct call of
twiki-org-outline-level, given that we've bound outline-level to
it in the let; originally, that call had been to the (let version
of) outline-level, but I found it was calling the function
outline-level (instead of the twiki-org version) instead.

FIXME: Not sure why the original function went all the way back
to the beginning of the buffer to figure out the level."
  (let ((count 1)
        (outline-level 'twiki-org-outline-level))
    (save-excursion
      (outline-back-to-heading t)
      (twiki-org-outline-level))))
      ;; (while (and (not (bobp))
      ;;             (not (eq (twiki-org-outline-level) 1)))
      ;;   (outline-up-heading 1)
      ;;   (or (bobp)
      ;;       (setq count (1+ count))))
      ;; count)))

(defun twiki-org-outline-level ()
  "Return the depth to which a statement is nested in the outline.
Point must be at the beginning of a header line.
This is actually either the level specified in `outline-heading-alist'
or else the number of characters matched by `outline-regexp'.

Customized for twiki-org-mode."
  (or (cdr (assoc (match-string 1) outline-heading-alist))
      (- (match-end 1) (match-beginning 1))))

(defun twiki-org-level-interactive ()
  "Interactive call for twiki-org-level."
  (interactive)
  (let ((level (twiki-org-level)))
    (message "Level %s, heading %s" level (concat "---" (make-string level ?+) " "))))

(defun twiki-org-promo()
  "Interactive call for twiki-org-promote."
  (interactive)
  (save-excursion
    (twiki-org-promote)))

(defun twiki-org-do-promote ()
  "Promote the current heading higher up the tree.
If the region is active in `transient-mark-mode', promote all headings
in the region.

FIXME: The org function that calls hooks expects success; the
simplest way I've found to do that is the stupid bit of math at
the end.  I'm sure this is quite wrong."
  (interactive)
  (save-excursion
    (if (org-region-active-p)
	(org-map-region 'twiki-org-promote (region-beginning) (region-end))
      (twiki-org-promote)))
  (org-fix-position-after-promote)
  (+ 1 1))

(defun twiki-org-do-demote ()
  "Demote the current heading lower down the tree.
If the region is active in `transient-mark-mode', demote all headings
in the region.

FIXME: The org function that calls hooks expects success; the
simplest way I've found to do that is the stupid bit of math at
the end.  I'm sure this is quite wrong."
  (interactive)
  (save-excursion
    (if (org-region-active-p)
	(org-map-region 'twiki-org-demote (region-beginning) (region-end))
      (twiki-org-demote)))
  (org-fix-position-after-promote)
  (+ 1 1))


(defun twiki-org-promote ()
  "Promote the current heading higher up the tree.
If the region is active in `transient-mark-mode', promote all headings
in the region."
  (org-back-to-heading t)
  (let* ((level (save-match-data (funcall 'twiki-org-level)))
	 (after-change-functions (remove 'flyspell-after-change-function
					  after-change-functions))
	 (up-head (concat "---" (make-string (- level 1) ?+) " "))
	 (diff (abs (- level (length up-head) -1))))
    (if (= level 1) (error "Cannot promote to level 0.  UNDO to recover if necessary"))
    (replace-match up-head nil t)
    ;; Fixup tag positioning
    (and org-auto-align-tags (org-set-tags nil t))
    (if org-adapt-indentation (org-fixup-indentation (- diff)))
    (run-hooks 'org-after-promote-entry-hook)))

(defun twiki-org-dmo ()
  "Interactive call for twiki-org-promote."
  (interactive)
  (save-excursion
    (twiki-org-demote)))

(defun twiki-org-metaright-hook ()
  "Hook to get meta-right to run twiki-org-demote."
  (cond
   ((or (org-at-item-p)
	(and (org-region-active-p)
	     (save-excursion
	       (goto-char (region-beginning))
	       (org-at-item-p))))
    (call-interactively 'twiki-org-indent-item))
   ((or (org-on-heading-p)
       (and (org-region-active-p)
	    (save-excursion
	      (goto-char (region-beginning))
	      (org-on-heading-p)))
       (call-interactively 'twiki-org-do-demote)))))


;; (defun twiki-org-indent-item ()
;;   "Meant to indent a Twiki item -- lists, in particular. FIXME: Not written yet.

;; Currently calling org-interactive item.  May be able to get away
;; with twiddling orgg-item-indent-positions in order to get
;; indentation right (multiple of 3)."
;;   (interactive)
;;   (message "FIXME: Not written yet...calling org-interactive-item")
;;   (call-interactively 'org-indent-item))

(defun twiki-org-indent-item (arg)
  "Indent a local list item. Copied from org-indent-item"
  (interactive "p")
  (and (org-region-active-p) (org-cursor-to-region-beginning))
  (unless (org-at-item-p)
    (error "Not on an item"))
  (let (beg end ind ind1 ind-bul delta ind-down ind-up firstp)
    (setq firstp (org-first-list-item-p))
    (save-excursion
      (setq end (and (org-region-active-p) (region-end)))
      (if (memq last-command '(org-shiftmetaright org-shiftmetaleft))
	  (setq beg org-last-indent-begin-marker
		end org-last-indent-end-marker)
	(org-beginning-of-item)
	(setq beg (move-marker org-last-indent-begin-marker (point)))
	(org-end-of-item)
	(setq end (move-marker org-last-indent-end-marker (or end (point)))))
      (goto-char beg)
      (setq ind-bul (twiki-org-item-indent-positions)
	    ind (caar ind-bul)
	    ind-down (car (nth 2 ind-bul))
	    ind-up (car (nth 1 ind-bul))
	    delta (if (> arg 0)
		      (if ind-down (- ind-down ind) 2)
		    (if ind-up (- ind-up ind) -2)))
      (if (< (+ delta ind) 0) (error "Cannot outdent beyond margin"))
      (while (< (point) end)
	(beginning-of-line 1)
	(skip-chars-forward " \t") (setq ind1 (current-column))
	(delete-region (point-at-bol) (point))
	(or (eolp) (org-indent-to-column (+ ind1 delta)))
	(beginning-of-line 2)))
    (org-fix-bullet-type
     (and (> arg 0)
	  (not firstp)
	  (cdr (assoc (cdr (nth 0 ind-bul)) org-list-demote-modify-bullet))))
    (org-maybe-renumber-ordered-list-safe)
    (save-excursion
      (beginning-of-line 0)
      (condition-case nil (org-beginning-of-item) (error nil))
      (org-maybe-renumber-ordered-list-safe))))

(defun twiki-org-item-indent-positions ()
  "Return indentation for plain list items.
This returns a list with three values:	The current indentation, the
parent indentation and the indentation a child should have.
Assumes cursor in item line."
  (let* ((bolpos (point-at-bol))
	 (ind (org-get-indentation))
	 (bullet (org-get-bullet))
	 ind-down ind-up bullet-up bullet-down pos)
    (save-excursion
      (org-beginning-of-item-list)
      (skip-chars-backward "\n\r \t")
      (when (org-in-item-p)
	(org-beginning-of-item)
	(setq ind-up (org-get-indentation))
	(setq bullet-up (org-get-bullet))))
    (setq pos (point))
    (save-excursion
      (cond
       ((and (condition-case nil (progn (org-previous-item) t)
	       (error nil))
	     (or (forward-char 1) t)
	     (re-search-forward "^\\([ \t]*\\([-+]\\|\\([0-9]+[.)]\\)\\)\\|[ \t]+\\*\\)\\( \\|$\\)" bolpos t))
	(setq ind-down (org-get-indentation)
	      bullet-down (org-get-bullet)))
       ((and (goto-char pos)
	     (org-at-item-p))
	(goto-char (match-end 0))
	(skip-chars-forward " \t")
	;; (setq ind-down (current-column)
	;; Hack: next multiple of 3 like so:
	;; current column + 3, divided by 3, truncated, times 3.
	;; This is a big ugly, and I'm not sure if I need to call truncate;
	;; doesn't look like it's floating point by default. But I'll leave
	;; it in here for now.
	(setq ind-down (* 3 (truncate (/ (+ 3 (current-column)) 3)))
	      bullet-down (org-get-bullet)))))
    (if (and bullet-down (string-match "\\`[0-9]+\\(\\.\\|)\\)\\'" bullet-down))
	(setq bullet-down (concat "1" (match-string 1 bullet-down))))
    (if (and bullet-up (string-match "\\`[0-9]+\\(\\.\\|)\\)\\'" bullet-up))
	(setq bullet-up (concat "1" (match-string 1 bullet-up))))
    (if (and bullet (string-match "\\`[0-9]+\\(\\.\\|)\\)\\'" bullet))
	(setq bullet (concat "1" (match-string 1 bullet))))
    (list (cons ind bullet)
	  (cons ind-up bullet-up)
	  (cons ind-down bullet-down))))


(defun twiki-org-metaleft-hook ()
  "Hook to get meta-left to run twiki-org-promote."
  (or (org-on-heading-p)
      (and (org-region-active-p)
	   (save-excursion
	     (goto-char (region-beginning))
	     (org-on-heading-p))))
  (call-interactively 'twiki-org-do-promote))

(defun twiki-org-demote ()
  "Demote the current heading lower down the tree.
If the region is active in `transient-mark-mode', demote all headings
in the region."
  (org-back-to-heading t)
  (let* ((level (save-match-data (funcall 'twiki-org-level)))
	 ;; (down-head (concat (make-string (org-get-valid-level level 1) ?*) " "))
	 (down-head  (concat "---" (make-string (+ level 1) ?+) " "))
	 (diff (abs (- level (length down-head) -1))))
    (replace-match down-head nil t)
    ;; Fixup tag positioning
    (and org-auto-align-tags (org-set-tags nil t))
    (if org-adapt-indentation (org-fixup-indentation diff))
    (run-hooks 'org-after-demote-entry-hook)))

(defun twiki-org-choose-face ()
  (let ((face (aref twiki-org-faces
                    (% (funcall outline-level)
                       (length twiki-org-faces)))))
    (if (facep face)
        face
      (aref twiki-org-faces 0))))

;;;###autoload
(defun twiki-org-toggle-html (&optional prefix)
  (interactive "P")
  (setq twiki-org-hide-html-tags
        (if prefix
            (>= (prefix-numeric-value prefix) 0)
          (not twiki-org-hide-html-tags)))

  (cond (twiki-org-hide-html-tags
         (add-to-list 'buffer-invisibility-spec 'twiki-org-html-tag))
        (t
         (setq buffer-invisibility-spec
               (delq 'twiki-org-html-tag buffer-invisibility-spec))))

  (and (called-interactively-p "interactive")
       (if twiki-org-hide-html-tags
           (message "Hiding HTML tags")
         (message "Displaying HTML tags"))))

;;;###autoload
(define-derived-mode twiki-org-mode org-mode "TwikiOrg" nil
  (make-local-variable 'outline-regexp)
  (setq outline-regexp twiki-org-regexp)

  (make-local-variable 'outline-level)
  (setq outline-level 'twiki-org-level)

  (make-local-variable font-lock-defaults)
  (setq font-lock-defaults twiki-org-font-lock-keywords)

  (twiki-org-toggle-html twiki-org-hide-html-tags)

;  (local-set-key "C-cu" 'twiki-org-promote)
;  (local-set-key [f9] 'twiki-org-toggle-html)
  (add-hook 'org-metaright-hook 'twiki-org-metaright-hook)
  (add-hook 'org-metaleft-hook  'twiki-org-do-promote)

  (make-local-variable 'font-lock-extra-managed-props)
  (setq font-lock-extra-managed-props '(invisible)))

(provide 'twiki-org)

(run-hooks 'twiki-org-load-hook)

;; FIXME: Should we be using org-fontify-like-in-org-mode?  See org.el.
;; FIXME: Should we be using org-insert-link-global?  See org.el.

;;; twiki-org.el ends here.
