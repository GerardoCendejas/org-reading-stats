(require 'simple-httpd)
(require 'json)

(defvar org-reading-stats-file "~/.emacs.d/org/read_papers.org"
  "Path to your org reading file.")

(defun org-reading-stats-generate-json ()
  "Process the file using simple string matching instead of complex regex."
  (interactive)
  (let* ((base (file-name-directory (or load-file-name (buffer-file-name) default-directory)))
         (web-dir (expand-file-name "web/" base))
         (results nil)
         (current-cite nil))
    
    (unless (file-directory-p web-dir)
      (make-directory web-dir t))

    (with-temp-buffer
      (insert-file-contents org-reading-stats-file)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
          (cond
           ;; 1. Look for the citation key
           ((string-match "cite:@\\([^]]+\\)" line)
            (setq current-cite (match-string 1 line)))

           ;; 2. Look for a date (YYYY-MM-DD) only if we have a pending citation
           ((and current-cite (string-match "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" line))
            (push `((cite . ,current-cite)
                    (timestamp . ,(match-string 1 line)))
                  results)
            (setq current-cite nil))))
        (forward-line 1)))

    (with-temp-file (expand-file-name "data.json" web-dir)
      (insert (json-encode (or results []))))
    
    (message "Analysis finished: %d papers found." (length results))))

(defun org-reading-stats-start ()
  "Starts the server and generates data."
  (interactive)
  (org-reading-stats-generate-json)
  (setq httpd-port 8080)
  (setq httpd-root (expand-file-name "web" (file-name-directory (or load-file-name (buffer-file-name) default-directory))))
  (httpd-start)
  (message "Dashboard ready at http://localhost:8080/index.html"))

(provide 'org-reading-stats)
