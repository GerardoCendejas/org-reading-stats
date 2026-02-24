(require 'simple-httpd)
(require 'json)

;; ADJUST THIS: Path to your org-roam notes
(defvar org-roam-directory-path "~/.emacs.d/org/roam/"
  "Path to your org-roam notes directory.")

(defvar org-reading-stats-file "~/.emacs.d/org/read_papers.org"
  "Path to your org reading file.")

(defun org-reading-stats-get-file-info (path)
  "Open the file briefly to count words and links."
  (with-temp-buffer
    (insert-file-contents path)
    (let ((word-count (count-words (point-min) (point-max)))
          (link-count 0))
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[" nil t)
        (setq link-count (1+ link-count)))
      (list word-count link-count))))

(defun org-reading-stats-generate-json ()
  "Process papers and extract depth info (words and links) from notes."
  (interactive)
  (let* ((script-dir (file-name-directory (or load-file-name (buffer-file-name) default-directory)))
         (web-dir (expand-file-name "web/" script-dir))
         (results nil))
    
    (unless (file-directory-p web-dir) (make-directory web-dir t))

    (with-temp-buffer
      (insert-file-contents org-reading-stats-file)
      (goto-char (point-min))
      (while (re-search-forward "@\\([^] \n\t]+\\)" nil t)
        (let* ((cite-id (match-string 1))
               (timestamp "")
               (has-note-val "no")
               (word-count 0)
               (link-count 0)
               (search-limit (save-excursion (forward-line 4) (point))))
          
          (save-excursion
            (when (re-search-forward "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}[^]>]*\\)" search-limit t)
              (setq timestamp (match-string 1))))
          
          (let ((matching-files (directory-files-recursively 
                                 org-roam-directory-path 
                                 (regexp-quote cite-id))))
            (when matching-files
              (let ((info (org-reading-stats-get-file-info (car matching-files))))
                (setq has-note-val "yes")
                (setq word-count (car info))
                (setq link-count (cadr info)))))
          
          (push (list (cons 'cite (concat "@" cite-id))
                      (cons 'timestamp timestamp)
                      (cons 'has_note has-note-val)
                      (cons 'words word-count)
                      (cons 'links link-count))
                results)))
      
      (with-temp-file (expand-file-name "data.json" web-dir)
        (let ((json-encoding-pretty-print t))
          (insert (json-encode (reverse results)))))
      
      (message "Dashboard updated: %d papers. Note Depth: %d total words." 
               (length results)
               (apply '+ (mapcar (lambda (x) (cdr (assoc 'words x))) results))))))

(defun org-reading-stats-start ()
  "Start the server."
  (interactive)
  (org-reading-stats-generate-json)
  (setq httpd-port 8087)
  (setq httpd-root (expand-file-name "web/" (file-name-directory (or load-file-name (buffer-file-name) default-directory))))
  (httpd-start)
  (browse-url "http://localhost:8087/index.html"))

(provide 'org-reading-stats)
