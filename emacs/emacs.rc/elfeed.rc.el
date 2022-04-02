;;; elfeed.rc.el --- Configuration for RSS feeds            -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Vojtech Stepancik

;; Author: Vojtech Stepancik <adalbert@AdalbertDEV>
;; Keywords: languages

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
(eval-when-compile
  (require 'use-package)
  (require 'cl-lib))

(use-package elfeed
  :straight t
  :custom
  (elfeed-feeds '()) ; removed for privacy
  (elfeed-search-filter "@2-months-ago -junk +unread")
  (vs/elfeed-ignored '())
  :hook
  (elfeed-new-entry . vs/--elfeed-retag-posts)
  (elfeed-show-mode
   . (lambda ()
       (setq-local shr-external-rendering-functions
                   (append
                    '((a . vs/--render-a))
                    shr-external-rendering-functions))))
  :init
  (defvar vs/elfeed-ignored nil
    "Alist of mapping from feed ID to a list of ignored categories in DNF.")
  (defvar vs/elfeed-junk-tag 'junk
    "Tag to add to entries that should be ignored.")
  :config
  (defun vs/--render-a (a)
    "Render A, with special casing for images.

If the content of a link is an image, remove the link from the image,
and append a text that says \"Link\" after the image."
    (when-let* ((inner (dom-children a))
                (img (and (eq 'img
                              (dom-tag inner))
                          (car inner))))
      (shr-tag-img img)
      (setq a (dom-node 'a
                        (dom-attributes a)
                        (concat
                         "("
                         (or
                          (when-let ((alt (dom-attr img 'alt)))
                            (and (stringp alt)
                                 (not (string-empty-p alt))
                                 alt))
                          "Link")
                         ")"))))
    (vs/--elfeed-local-href a))
  (defun vs/--elfeed-local-href (a)
    "Render A, correctly handling local references."
    (let ((href (dom-attr a 'href))
          (start (point)))
      (if (eq (string-to-char href)
              ?#)
          (progn
            (shr-generic a)
            (put-text-property
             start (point)
             'keymap vs/--elfeed-local-href-map)
            (shr-urlify start href (dom-attr a 'title)))
        (shr-tag-a a))))

  (defun vs/--elfeed-local-href-browse (&rest args)
    (interactive)
    (let ((url (get-text-property (point) 'shr-url)))
      (if (and url
               (eq (string-to-char url) ?#))
          (progn
            (goto-char (point-min))
            (unless (text-property-search-forward
                     'shr-target-id (substring url 1))))
        (message "No local link under point"))))

  (defvar vs/--elfeed-local-href-map
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map shr-map)
      (define-key map [remap shr-browse-url] #'vs/--elfeed-local-href-browse)
      map))

  (defun vs/--elfeed-should-be-junk (categories pattern)
    (cond
     ((stringp pattern) (member pattern categories))
     ((symbolp pattern) (vs/--elfeed-should-be-junk categories (symbol-name pattern)))
     ((listp pattern) (cl-loop for p in pattern
                               always (vs/--elfeed-should-be-junk categories p)))
     (t (user-error "Invalid ignore type"))))
  (defun vs/--elfeed-retag-posts (entry)
    (when-let* ((meta (elfeed-entry-meta entry))
                (id (elfeed-entry-feed-id entry))
                (ignored (alist-get id vs/elfeed-ignored nil nil #'equal))
                (categories (plist-get meta :categories)))
      (when (cl-loop for p in ignored
                     thereis (vs/--elfeed-should-be-junk categories p))
        (message "Tagging")
        (elfeed-tag-1 entry vs/elfeed-junk-tag))))
  (defun vs/elfeed-retag-all ()
    (interactive)
    (with-elfeed-db-visit (entry feed)
      (vs/--elfeed-retag-posts entry)))
  (require 'evil-collection)
  (evil-collection-elfeed-setup))

(provide 'elfeed.rc)
;;; elfeed.rc.el ends here
