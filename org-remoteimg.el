;;; org-remoteimg.el --- Display remote inline images in org-mode -*- lexical-binding: t -*-

;; Copyright (C) 2023 Dean Gao - MIT License
;; Author: Dean Gao <gao.dean@hotmail.com>
;; Description: Inline display of remote images in org-mode
;; Homepage: https://github.com/gaoDean/org-imgtog
;; Package-Requires: ((emacs "25.1"))

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; This package displays remote images inline in org-mode with automatic caching.
;; This means you can do [[https://my-file.png]], and have it display inline.
;; The next time you visit the file or fetch the image, it will be instantly
;; fetched from the cache.

;;; Code:

(require 'org)
(require 'org-element)
(require 'url)
(require 'url-cache)

(unless (fboundp 'image-supported-file-p)
  ;; `image-supported-file-p' isn't available before Emacs 28
  ;; Add alias to not break Emacs <28.
  (defalias 'image-type-from-file-name 'image-supported-file-p))

(defun org-image-update-overlay (file link &optional data-p refresh)
  "Create image overlay for FILE associtated with org-element LINK.
If DATA-P is non-nil FILE is not a file name but a string with the image data.
If REFRESH is non-nil don't download the file but refresh the image.
See also `create-image'.
This function is almost a duplicate of a part of `org-display-inline-images'."
  (when (or data-p (file-exists-p file))
    (let ((width
           ;; Apply `org-image-actual-width' specifications.
           (cond
            ((eq org-image-actual-width t) nil)
            ((listp org-image-actual-width)
             (or
              ;; First try to find a width among
              ;; attributes associated to the paragraph
              ;; containing link.
              (let ((paragraph
                     (let ((e link))
                       (while (and (setq e (org-element-property
                                            :parent e))
                                   (not (eq (org-element-type e)
                                            'paragraph))))
                       e)))
                (when paragraph
                  (save-excursion
                    (goto-char (org-element-property :begin paragraph))
                    (when
                        (re-search-forward
                         "^[ \t]*#\\+attr_.*?: +.*?:width +\\(\\S-+\\)"
                         (org-element-property
                          :post-affiliated paragraph)
                         t)
                      (string-to-number (match-string 1))))))
              ;; Otherwise, fall-back to provided number.
              (car org-image-actual-width)))
            ((numberp org-image-actual-width)
             org-image-actual-width)))
          (old (get-char-property-and-overlay
                (org-element-property :begin link)
                'org-image-overlay)))
      (if (and (car-safe old) refresh)
          (image-flush (overlay-get (cdr old) 'display))
        (let ((image (create-image file
                                   (and (image-type-available-p 'imagemagick)
                                        width
                                        'imagemagick)
                                   data-p
                                   :width width)))
          (when image
            (let* ((link
                    ;; If inline image is the description
                    ;; of another link, be sure to
                    ;; consider the latter as the one to
                    ;; apply the overlay on.
                    (let ((parent
                           (org-element-property :parent link)))
                      (if (eq (org-element-type parent) 'link)
                          parent
                        link)))
                   (ov (make-overlay
                        (org-element-property :begin link)
                        (progn
                          (goto-char
                           (org-element-property :end link))
                          (skip-chars-backward " \t")
                          (point)))))
              (overlay-put ov 'display image)
              (overlay-put ov 'face 'default)
              (overlay-put ov 'org-image-overlay t)
              (overlay-put
               ov 'modification-hooks
               (list 'org-display-inline-remove-overlay))
              (push ov org-inline-image-overlays)
              ov)))))))

(defun org-display-user-inline-images (&optional _include-linked _refresh beg end)
  "Like `org-display-inline-images' but for image data links.
_INCLUDE-LINKED and _REFRESH are ignored.
Restrict to region between BEG and END if both are non-nil.
Image data links have a :image-data-fun parameter.
\(See `org-link-set-parameters'.)
The value of the :image-data-fun parameter is a function
taking the PROTOCOL, the LINK, and the DESCRIPTION as arguments.
If that function returns nil the link is not interpreted as image.
Otherwise the return value is the image data string to be displayed.

Note that only bracket links are allowed as image data links
with one of the formats
 [[PROTOCOL:LINK]]
or
 [[PROTOCOL:LINK][DESCRIPTION]]
are recognized.
Full credit goes to org-yt by Tobias Zawada for this function."
  (interactive)
  (when (and (called-interactively-p 'any)
             (use-region-p))
    (setq beg (region-beginning)
          end (region-end)))
  (when (display-graphic-p)
    (org-with-wide-buffer
     (goto-char (or beg (point-min)))
     (when-let ((image-data-link-parameters
		 (cl-loop for link-par-entry in org-link-parameters
			  with fun
			  when (setq fun (plist-get (cdr link-par-entry) :image-data-fun))
			  collect (cons (car link-par-entry) fun)))
		(image-data-link-re (regexp-opt (mapcar 'car image-data-link-parameters)))
		(re (format "\\[\\[\\(%s\\):\\([^]]+\\)\\]\\(?:\\[\\([^]]+\\)\\]\\)?\\]"
			    image-data-link-re)))
       (while (re-search-forward re end t)
         (let* ((protocol (match-string-no-properties 1))
		(link (match-string-no-properties 2))
		(description (match-string-no-properties 3))
		(image-data-link (assoc-string protocol image-data-link-parameters))
		(el (save-excursion (goto-char (match-beginning 1)) (org-element-context)))
		image-data)
           (when el
             (setq image-data
                   (or (let ((old (get-char-property-and-overlay
                                   (org-element-property :begin el)
                                   'org-image-overlay)))
                         (and old
                              (car-safe old)
                              (overlay-get (cdr old) 'display)))
		       (funcall (cdr image-data-link) protocol link description)))
             (when image-data
               (let ((ol (org-image-update-overlay image-data el t t)))
                 (when (and ol description)
                   (overlay-put ol 'after-string description)))))))))))

(defun org-remoteimg--fetch-image (protocol link _description)
  "Synchronously retrieve image from cache or web"
  (when (and (image-supported-file-p link)
             (not (eq org-display-remote-inline-images 'skip)))
    (let* ((cache (eq org-display-remote-inline-images 'cache))
           (user-url-caching-setting url-automatic-caching)
           (url (concat protocol ":" link))
           (silent-output (file-exists-p (url-cache-create-filename url))))
      (when cache (setq url-automatic-caching t))
      (prog1
          (if-let (buf (url-retrieve-synchronously url
                                                   silent-output
                                                   nil
                                                   30))
              (with-current-buffer buf
                (goto-char (point-min))
                (re-search-forward "\r?\n\r?\n" nil t)
                (buffer-substring-no-properties (point) (point-max)))
            (message "Download of image \"%s\" failed" link)
            nil)
        (when cache
          (setq url-automatic-caching user-url-caching-setting))))))

(advice-add #'org-display-inline-images :after #'org-display-user-inline-images)

(org-link-set-parameters "http"  :image-data-fun #'org-remoteimg--fetch-image)
(org-link-set-parameters "https" :image-data-fun #'org-remoteimg--fetch-image)



(provide 'org-remoteimg)

;;; org-remoteimg.el ends here
