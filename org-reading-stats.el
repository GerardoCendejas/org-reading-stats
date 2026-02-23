(require 'simple-httpd)
(require 'json)

;; ADJUST THIS: Point this to your actual org-roam notes folder
(defvar org-roam-directory-path "~/.emacs.d/org/roam/" 
  "Path to your org-roam notes directory.")

(defvar org-reading-stats-file "~/.emacs.d/org/read_papers.org"
  "Path to your org reading file.")

(defun org-reading-stats-generate-json ()
  "Process papers and check for note existence by looking for physical files."
  (interactive)
  (let* ((script-dir (file-name-directory (or load-file-name (buffer-file-name) default-directory)))
         (web-dir (expand-file-name "web/" script-dir))
         (results nil))
    
    (unless (file-directory-p web-dir) (make-directory web-dir t))

    (with-temp-buffer
      (insert-file-contents org-reading-stats-file)
      (goto-char (point-min))
      (while (re-search-forward "@\\([^] \n\t]+\\)" nil t)
        (let* ((cite-id (match-string 1)) ;; e.g., "enbodyCommunitywide..."
               (timestamp "")
               (has-note-val "no")
               (search-limit (save-excursion (forward-line 4) (point))))
          
          ;; 1. Capture timestamp
          (save-excursion
            (when (re-search-forward "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}[^]>]*\\)" search-limit t)
              (setq timestamp (match-string 1))))
          
          ;; 2. NEW METHOD: Check if any file in org-roam directory contains the ID in its name
          ;; This is much more reliable than database queries if the DB is acting up.
          (let ((matching-files (directory-files-recursively 
                                 org-roam-directory-path 
                                 (regexp-quote cite-id))))
            (when matching-files
              (setq has-note-val "yes")))
          
          (push (list (cons 'cite (concat "@" cite-id))
                      (cons 'timestamp timestamp)
                      (cons 'has_note has-note-val))
                results)))
      
      (with-temp-file (expand-file-name "data.json" web-dir)
        (let ((json-encoding-pretty-print t))
          (insert (json-encode (reverse results)))))
      
      (message "Dashboard updated: %d papers. Found files for: %d" 
               (length results)
               (length (seq-filter (lambda (x) (string= (cdr (assoc 'has_note x)) "yes")) results))))))

(defun org-reading-stats-start ()
  "Start the server."
  (interactive)
  (org-reading-stats-generate-json)
  (setq httpd-port 8087)
  (setq httpd-root (expand-file-name "web/" (file-name-directory (or load-file-name (buffer-file-name) default-directory))))
  (httpd-start)
  (browse-url "http://localhost:8087/index.html"))

(provide 'org-reading-stats)
