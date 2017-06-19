;;; op-enhance.el --- HTML page customization required by org-page
;;; Commentary:
;; Improve generated html page display effect

;;; Code:

(require 'format-spec)
(require 'ox)
(require 'ht)
(require 'op-util)
(require 'op-vars)


(defun op/get-theme-dir ()
  "Return the resource storage directory, it is determined by variable
`op/theme-root-directory' and `op/theme'."
  (file-name-as-directory
   (expand-file-name
    (format "%s/resources" (symbol-name op/theme))
    op/theme-root-directory)))

(defun op/prepare-theme (pub-root-dir)
  "Copy theme files to PUB-ROOT-DIR."
  (let ((pub-theme-dir (expand-file-name "media/" pub-root-dir))
        (theme-dir (op/get-theme-dir)))
    (unless (file-directory-p theme-dir)
      (message "Theme %s not found, use default theme `mdo' instead."
               (symbol-name op/theme))
      (setq op/theme-root-directory (concat op/load-directory "themes/"))
      (setq op/theme 'mdo)
      (setq theme-dir (op/get-theme-dir)))
    (when (file-directory-p pub-theme-dir)
      (delete-directory pub-theme-dir t))
    (copy-directory theme-dir pub-theme-dir t t t)))


(provide 'op-enhance)

;;; op-enhance.el ends here
