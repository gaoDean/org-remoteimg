;;; org-remoteimg.el --- Display remote inline images in org-mode -*- lexical-binding: t -*-

;; Copyright (C) 2023 Dean Gao - MIT License
;; Author: Dean Gao <gao.dean@hotmail.com>
;; Description: Inline display of remote images in org-mode with automatic caching
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

;; This package displays remote images inline in org-mode with caching.
;; This means you can do [[https://my-file.png]], and have it display inline.
;; This image will also be cached for instant fetching next time.

;;; Code:

(require 'org)
(require 'url)
(require 'url-cache)

(defgroup org-remoteimg nil
  "Display remote images inline, with automatic caching."
  :group 'org)

(defcustom org-remoteimg-cache t
  "Sets if the images should be cached."
  :group 'org-remoteimg
  :type 'boolean)

(defcustom org-remoteimg-cache-expire 168
"Time (in hours) after which a cached image is considered expired.
Defaults to one week.
This is useful because some images are constantly updated in
certain websites."
  :group 'org-remoteimg
  :type 'number)

(defun org-remoteimg--extract-image-data ()
  "Extract image data from HTTP response."
  (goto-char (point-min))
  (re-search-forward "\r?\n\r?\n" nil t)
  (buffer-substring-no-properties (point) (point-max)))

(defun org-remoteimg--fetch-image (protocol link _description)
  "Interpret LINK as an URL to an image file."
  (when (and (image-type-from-file-name link))
    (let* ((url (concat protocol ":" link))
           (cache-file (url-cache-create-filename url))
           (url-cache-settings url-automatic-caching))
      (if (and (file-exists-p cache-file)
               (not (url-cache-expired url (* 60 60 org-remoteimg-cache-expire))))
          (with-temp-buffer
            (set-buffer-multibyte nil) ; Ensure the buffer is unibyte
            (url-cache-extract cache-file) ; Insert the cache data
            (org-remoteimg--extract-image-data))
        (prog2
            (setq url-automatic-caching org-remoteimg-cache)
            (if-let (buf (url-retrieve-synchronously url))
                (with-current-buffer buf
                  (org-remoteimg--extract-image-data))
              (message "Download of image \"%s\" failed" link) nil)
            (setq url-automatic-caching url-cache-settings))))))

(org-link-set-parameters "http"  :image-data-fun #'org-remoteimg--fetch-image)
(org-link-set-parameters "https" :image-data-fun #'org-remoteimg--fetch-image)


(provide 'org-remoteimg)

;;; org-remoteimg.el ends here
