;;; ytdious.rc.el --- Configuration for YTdious      -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Vojtech Stepancik

;; Author: Vojtech Stepancik <vojtech.stepancik.2e@stu.hosei.ac.jp>
;; Keywords: multimedia, hypermedia

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

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package ytdious
  :straight t
  :hook
  (ytdious-mode . hl-line-mode)
  :custom
  (ytdious-invidious-api-url "https://invidious.snopyta.org")
  :init
  (defsubst vs/--ytdious-url-from-id (id)
    "Format the video ID into an URL."
    (concat "https://www.youtube.com/watch?v=" id))
  (defun vs/ytdious-play-mpv ()
    "Play the video under point in MPV."
    (interactive)
    (when-let* ((video (ytdious-get-current-video))
                (id (ytdious-video-id-fun video)))
      (when-let ((name (alist-get 'title video)))
        (message "Playing: %s" name))
      (vs/mpv-play (vs/--ytdious-url-from-id id))))
  (defun vs/ytdious-copy-url ()
    "Copy the url of the video under point."
    (interactive)
    (when-let* ((video (ytdious-get-current-video))
                (id (ytdious-video-id-fun video)))
      (when-let ((name (alist-get 'title video)))
        (message "Copied URL of: %s" name))
      (kill-new (vs/--ytdious-url-from-id id)))))

(provide 'ytdious.rc)
;;; ytdious.rc.el ends here
