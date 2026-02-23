(require 'simple-httpd)
(require 'json)

(defvar org-reading-stats-file "~/.emacs.d/org/read_papers.org"
  "Path to your org reading file.")

(defun org-reading-stats-generate-json ()
  "Robust block-based processing to ensure cite and date are paired correctly."
  (interactive)
  (let* ((script-dir (file-name-directory (or load-file-name (buffer-file-name) default-directory)))
         (web-dir (expand-file-name "web/" script-dir))
         (results nil))
    
    (unless (file-directory-p web-dir) (make-directory web-dir t))

    (with-temp-buffer
      (insert-file-contents org-reading-stats-file)
      (goto-char (point-min))
      ;; Search for every instance of a citation key
      (while (re-search-forward "@\\([^] \n\t]+\\)" nil t)
        (let* ((cite-key (match-string 0))
               (cite-id (match-string 1))
               (timestamp nil)
               (has-note nil)
               (search-limit (save-excursion (forward-line 3) (point)))) ; Look up to 3 lines ahead for date
          
          ;; Look for the timestamp right after the citation
          (save-excursion
            (when (re-search-forward "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}[^]>]*\\)" search-limit t)
              (setq timestamp (match-string 1))))
          
          ;; Check Org-roam for the note
          (setq has-note (if (and (featurep 'org-roam) (org-roam-db-sync))
                             (or (org-roam-node-from-ref cite-key)
                                 (seq-find (lambda (node) 
                                             (string= (org-roam-node-title node) cite-id))
                                           (org-roam-node-list)))
                           nil))
          
          ;; Only push to results if we found at least a citation
          (when cite-key
            (push `((cite . ,cite-key)
                    (timestamp . ,(or timestamp ""))
                    (has_note . ,(if has-note t :json-false))) ; Use :json-false for explicit boolean
                  results))))
      
      ;; Write the final JSON
      (with-temp-file (expand-file-name "data.json" web-dir)
        (insert (json-encode (reverse results))))
      (message "Dashboard updated: %d papers processed." (length results)))))

(defun org-reading-stats-start ()
  "Start the local server and refresh data."
  (interactive)
  (org-reading-stats-generate-json)
  (setq httpd-port 8087)
  (setq httpd-root (expand-file-name "web/" (file-name-directory (or load-file-name (buffer-file-name) default-directory))))
  (httpd-start)
  (browse-url "http://localhost:8087/index.html"))
