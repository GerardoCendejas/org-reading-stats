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

;;; ================= PARSER NATIVO Y SEGURO DE ENTRADAS BIBTEX =================

(defun org-reading-stats-get-raw-field-bounds (end-entry field)
  "Busca un FIELD de manera case-insensitive y devuelve un cons (start . end) con su contenido crudo."
  (save-excursion
    (let ((case-fold-search t)
          (found nil))
      ;; Buscamos el campo seguido de un signo '=' (ej: title= o author  =)
      (while (and (not found) (re-search-forward (concat "\\b" field "[ \t]*=") end-entry t))
        ;; Nos aseguramos de saltar espacios en blanco hasta el delimitador
        (skip-chars-forward " \t\n\r")
        (let ((delim (char-after)))
          (cond
           ;; Caso 1: Delimitado por llaves `{...}`
           ((eq delim ?{)
            (forward-char 1)
            (let ((start (point)))
              (backward-char 1)
              (ignore-errors
                (forward-sexp 1)
                (setq found (cons start (1- (point)))))))
           ;; Caso 2: Delimitado por comillas `\"...\"`
           ((eq delim ?\")
            (forward-char 1)
            (let ((start (point)))
              (when (search-forward "\"" end-entry t)
                (setq found (cons start (1- (point))))))))))
      found)))

(defun org-reading-stats-extract-metadata-from-file (bib-file cite-key)
  "Extrae title, author y páginas de una cita en un archivo BIB de manera nativa y segura."
  (if (and bib-file (file-exists-p bib-file))
      (with-temp-buffer
        (insert-file-contents bib-file)
        (goto-char (point-min))
        (let ((clean-key (if (string-prefix-p "@" cite-key) (substring cite-key 1) cite-key)))
          ;; Localizamos la entrada de la cita en el archivo .bib
          (if (re-search-forward (concat "@[a-zA-Z]+{[ \t]*" (regexp-quote clean-key) "[ \t]*,") nil t)
              (let* ((end-entry (save-excursion (if (re-search-forward "^}" nil t) (point) (point-max))))
                     (is-book (save-excursion (goto-char (line-beginning-position)) (looking-at-p "@book")))
                     (title "")
                     (author "")
                     (pages (if is-book 250 15)))
                
                ;; 1. Extraer Title
                (let ((title-bounds (org-reading-stats-get-raw-field-bounds end-entry "title")))
                  (when title-bounds
                    (setq title (buffer-substring-no-properties (car title-bounds) (cdr title-bounds)))))
                
                ;; 2. Extraer Author
                (let ((author-bounds (org-reading-stats-get-raw-field-bounds end-entry "author")))
                  (when author-bounds
                    (setq author (buffer-substring-no-properties (car author-bounds) (cdr author-bounds)))))
                
                ;; 3. Extraer Páginas y calcular el conteo
                (let ((pages-bounds (org-reading-stats-get-raw-field-bounds end-entry "pages")))
                  (when pages-bounds
                    (let ((pstr (buffer-substring-no-properties (car pages-bounds) (cdr pages-bounds))))
                      (cond
                       ((string-match "\\([0-9]+\\)[- ]+\\([0-9]+\\)" pstr)
                        (setq pages (1+ (abs (- (string-to-number (match-string 2 pstr))
                                              (string-to-number (match-string 1 pstr)))))))
                       ((string-match "^[ \t]*\\([0-9]+\\)[ \t]*$" pstr)
                        (setq pages (string-to-number (match-string 1 pstr))))))))
                
                ;; Retornamos una lista con los valores limpios
                (list title author pages))
            nil)))
    nil))

(defun org-reading-stats-get-metadata (cite-key)
  "Recorre tus archivos .bib en cascada buscando los metadatos completos."
  (let ((bib-files (org-reading-stats-get-all-bibs))
        (found-meta nil))
    (while (and bib-files (not found-meta))
      (setq found-meta (org-reading-stats-extract-metadata-from-file (car bib-files) cite-key))
      (setq bib-files (cdr bib-files)))
    ;; Fallback seguro si no encuentra la entrada en tus .bib
    (or found-meta (list "" "" 10))))

;;; ============================================================================

(defun org-reading-stats-get-file-info (path)
  "Cuenta palabras (estrictamente post-configuración #+) y links en la nota de roam."
  (with-temp-buffer
    (insert-file-contents path)
    (let ((word-count 0)
          (link-count 0)
          (body-start (point-min)))
      
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^#\\+.*$" nil t)
          (setq body-start (match-end 0)))
        
        (goto-char body-start)
        (when (re-search-forward "[[:alnum:]]" nil t)
          (setq body-start (match-beginning 0))))

      (setq word-count (count-words body-start (point-max)))

      (goto-char (point-min))
      (while (re-search-forward "\\[\\[" nil t)
        (setq link-count (1+ link-count)))

      (list word-count link-count))))
      
(defun org-reading-stats-generate-json ()
  "Genera JSON con soporte para relecturas, detección de hora y metadatos reales de títulos y autores."
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
               ;; Obtenemos todos los metadatos juntos en una sola pasada nativa ultra rápida
               (metadata (org-reading-stats-get-metadata cite-key))
               (title-val (nth 0 metadata))
               (author-val (nth 1 metadata))
               (page-count (nth 2 metadata))
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
          
          ;; Limpieza profunda de los strings para que el JSON quede impecable
          (setq title-val (replace-regexp-in-string "[{}]" "" title-val))
          (setq title-val (replace-regexp-in-string "[ \t\n\r]+" " " title-val))
          (setq title-val (string-trim title-val))
          
          (setq author-val (replace-regexp-in-string "[{}]" "" author-val))
          (setq author-val (replace-regexp-in-string "[ \t\n\r]+" " " author-val))
          (setq author-val (string-trim author-val))

          (push (list (cons 'cite cite-key)
                      (cons 'title title-val)
                      (cons 'author author-val)
                      (cons 'timestamp timestamp)
                      (cons 'has_note has-note-val)
                      (cons 'words word-count)
                      (cons 'links link-count)
                      (cons 'pages page-count))
                results))))
    (with-temp-file (expand-file-name "data.json" web-dir)
      (let ((json-encoding-pretty-print t))
        (insert (json-encode (reverse results)))))
    (message "Dashboard actualizado: %d entradas con títulos y autores inyectados correctamente." (length results))))

(defun org-reading-stats-start ()
  "Start the local server and open the reading stats index."
  (interactive)
  (org-reading-stats-generate-json)
  (let* ((base-dir (file-name-directory (or load-file-name
                                            (buffer-file-name (get-buffer "org-reading-stats.el"))
                                            default-directory)))
         (web-path (expand-file-name "web/" base-dir)))
    (setq httpd-port 8087 
          httpd-root web-path)
    (httpd-start)
    (browse-url "http://localhost:8087/index.html")))

(provide 'org-reading-stats)
