(require 'simple-httpd)
(require 'json)

;; ADJUST THIS: Path to your org-roam notes
(defvar org-roam-directory-path "~/.emacs.d/org/roam/"
  "Path to your org-roam notes directory.")

(defvar org-reading-stats-file "~/.emacs.d/org/read_papers.org"
  "Path to your org reading file.")

(defvar org-reading-stats-extra-bib-files '("~/.emacs.d/org/bibliography/references.bib" "~/.emacs.d/org/bibliography/secondary_references.bib")
  "List of additional bibliography files to search for page counts.")

(defun org-reading-stats-get-all-bibs ()
  "Get all bib files from the Org header plus the extra ones."
  (let ((bibs '()))
    (with-temp-buffer
      (when (file-exists-p org-reading-stats-file)
        (insert-file-contents org-reading-stats-file)
        (goto-char (point-min))
        (while (re-search-forward "^#\\+BIBLIOGRAPHY:[ \t]*\\(.*?\\)$" nil t)
          (let ((path (match-string 1)))
            (setq bibs (append bibs (list (expand-file-name path))))))))
    (append bibs (mapcar #'expand-file-name org-reading-stats-extra-bib-files))))

(defun org-reading-stats-extract-pages-from-file (bib-file cite-key)
  "Attempt to find page count for CITE-KEY in a specific BIB-FILE."
  (if (and bib-file (file-exists-p bib-file))
      (with-temp-buffer
        (insert-file-contents bib-file)
        (goto-char (point-min))
        (let ((clean-key (if (string-prefix-p "@" cite-key) (substring cite-key 1) cite-key)))
          (if (re-search-forward (concat "@[a-zA-Z]+{" (regexp-quote clean-key) ",") nil t)
              (let ((end (save-excursion (re-search-forward "^}" nil t) (point)))
                    (is-book (save-excursion (goto-char (line-beginning-position)) (looking-at-p "@book")))
                    (pages 0))
                (if (re-search-forward "pages[ \t]*=[ \t]*{\\([^}]+\\)}" end t)
                    (let ((pstr (match-string 1)))
                      (cond
                       ((string-match "\\([0-9]+\\)[- ]+\\([0-9]+\\)" pstr)
                        (setq pages (1+ (abs (- (string-to-number (match-string 2 pstr))
                                              (string-to-number (match-string 1 pstr)))))))
                       ((string-match "^[ \t]*\\([0-9]+\\)[ \t]*$" pstr)
                        (setq pages (string-to-number (match-string 1 pstr))))
                       (t (setq pages 15))))
                  (setq pages (if is-book 250 15)))
                pages)
            nil)))
    nil))

(defun org-reading-stats-get-pages (cite-key)
  (let ((bib-files (org-reading-stats-get-all-bibs))
        (found-pages nil))
    (while (and bib-files (not found-pages))
      (setq found-pages (org-reading-stats-extract-pages-from-file (car bib-files) cite-key))
      (setq bib-files (cdr bib-files)))
    (or found-pages 10)))

(defun org-reading-stats-get-file-info (path)
  (with-temp-buffer
    (insert-file-contents path)
    (let ((word-count (count-words (point-min) (point-max)))
          (link-count 0))
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[" nil t)
        (setq link-count (1+ link-count)))
      (list word-count link-count))))

(defun org-reading-stats-generate-json ()
  "Genera JSON con soporte para relecturas y detección de hora."
  (interactive)
  (let* ((script-dir (file-name-directory (or load-file-name (buffer-file-name) default-directory)))
         (web-dir (expand-file-name "web/" script-dir))
         (results nil))
    (unless (file-directory-p web-dir) (make-directory web-dir t))
    (with-temp-buffer
      (insert-file-contents org-reading-stats-file)
      (goto-char (point-min))
      (while (re-search-forward "@\\([^] \n\t,]+\\)" nil t)
        (let* ((cite-id (match-string 1))
               (cite-key (concat "@" cite-id))
               (timestamp "")
               (has-note-val "no")
               (word-count 0)
               (link-count 0)
               (page-count (org-reading-stats-get-pages cite-key))
               (search-limit (save-excursion (forward-line 4) (point))))
          (save-excursion
            (when (re-search-forward "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}[^]>]*\\)" search-limit t)
              (setq timestamp (match-string 1))))
          (let ((matching-files (directory-files-recursively org-roam-directory-path (regexp-quote cite-id))))
            (when matching-files
              (let ((info (org-reading-stats-get-file-info (car matching-files))))
                (setq has-note-val "yes")
                (setq word-count (car info))
                (setq link-count (cadr info)))))
          (push (list (cons 'cite cite-key) (cons 'timestamp timestamp) (cons 'has_note has-note-val)
                      (cons 'words word-count) (cons 'links link-count) (cons 'pages page-count))
                results))))
    (with-temp-file (expand-file-name "data.json" web-dir)
      (let ((json-encoding-pretty-print t))
        (insert (json-encode (reverse results)))))
    (message "Dashboard actualizado: %d entradas." (length results))))

(defun org-reading-stats-start ()
  "Start the local server and open the reading stats index."
  (interactive)
  (org-reading-stats-generate-json)
  (let* ((base-dir (file-name-directory (or load-file-name ; <--- SIN PARÉNTESIS AQUÍ
                                            (buffer-file-name (get-buffer "org-reading-stats.el"))
                                            default-directory)))
         (web-path (expand-file-name "web/" base-dir)))
    (setq httpd-port 8087 
          httpd-root web-path)
    (httpd-start)
    (browse-url "http://localhost:8087/index.html")))

(provide 'org-reading-stats)
