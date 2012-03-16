;;; twiki-org.el --- major mode for editing twiki documents

;; My attempt to base twiki mode on org, rather than outline.
;; Hugh Brown, March 16 2012

;; Copyright (C) 2004 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Created: 2004-04-21

;; $Id: twiki-outline.el,v 1.1 2004/04/26 19:15:35 friedman Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
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

(defvar twiki-org-regexp "---+\\++ \\|\\(\\(?:   \\)+\\)[0-9*] ")

(defvar twiki-org-html-tag-face 'twiki-org-html-tag-face
  "Font Lock mode face used to highlight HTML tags in twiki-org mode.")

(defface twiki-org-html-tag-face
    '((t :inherit font-lock-string-face))
    "Font Lock mode face used to highlight HTML tags in twiki-org mode.")

(defvar twiki-org-hide-html-tags t)

(defvar twiki-org-font-lock-keywords
  '(("---+\\++ .*\\|\\(\\(?:   \\)+\\)[0-9*] .*\\(?:\n\\1 +[^ %0-9*\n].*\\)*"
     0 (twiki-org-choose-face) nil t)
    ("<[^\>]*>"
     0 '(face           twiki-org-html-tag-face
         invisible      twiki-org-html-tag
         read-only      twiki-org-html-tag
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
  (let ((count 1)
        (outline-level 'outline-level))
    (save-excursion
      (outline-back-to-heading t)
      (while (and (not (bobp))
                  (not (eq (outline-level) 1)))
        (outline-up-heading 1)
        (or (bobp)
            (setq count (1+ count))))
      count)))

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
         (add-to-list 'buffer-invisibility-spec 'twiki-org-html-tag)
         (when (listp inhibit-read-only)
           (setq inhibit-read-only
                 (delq 'twiki-org-html-tag inhibit-read-only))))
        (t
         (setq buffer-invisibility-spec
               (delq 'twiki-org-html-tag buffer-invisibility-spec))
         (when (listp inhibit-read-only)
           (add-to-list 'inhibit-read-only 'twiki-org-html-tag))))

  (and (interactive-p)
       (if twiki-org-hide-html-tags
           (message "Hiding HTML tags")
         (message "Displaying HTML tags"))))

;;;###autoload
(define-derived-mode twiki-org-mode org-mode "TwikiOrg" nil
  (make-local-variable 'outline-regexp)
  (setq outline-regexp twiki-org-regexp)

  (make-local-variable 'outline-level)
  (setq outline-level 'twiki-org-level)

  (setcar font-lock-defaults 'twiki-org-font-lock-keywords)

  (make-local-variable 'inhibit-read-only)
  (setq inhibit-read-only nil)

  (twiki-org-toggle-html twiki-org-hide-html-tags)

  (local-set-key "C-cu" 'twiki-org-promote)
  (make-local-variable 'font-lock-extra-managed-props)
  (setq font-lock-extra-managed-props '(invisible)))

(provide 'twiki-org)

;;; twiki-org.el ends here.
