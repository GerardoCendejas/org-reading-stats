(require 'simple-httpd)
(require 'json)

;; ADJUST THIS: Path to your org-roam notes
(defvar org-roam-directory-path "~/.emacs.d/org/roam/"
  "Path to your org-roam notes directory.")

(defvar org-reading-stats-file "~/.emacs.d/org/read_papers.org"
  "Path to your org reading file.")

(defvar org-reading-stats-extra-bib-files '("~/.emacs.d/org/bibliography/references.bib" "~/.emacs.d/org/bibliography/secondary_references.bib")
  "List of additional bibliography files to search for page counts.")

;; Ruta al archivo de mapeo de autores (sinónimos)
(defvar org-reading-stats-author-synonyms-file "~/.emacs.d/org/reading-stats/author_synonyms.txt"
  "Path to the tab-separated file containing author\\tsynonym mappings.")

;; LA SOLUCIÓN IGUAL A ORG-ROAM-STATS: Constante inmutable para la raíz del paquete
(defconst org-reading-stats--package-root
  (eval-and-compile
    (file-name-directory (or (bound-and-true-p byte-compile-current-file)
                             load-file-name
                             (buffer-file-name)
                             default-directory)))
  "Ruta absoluta inmutable del directorio raíz de este paquete.")

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

;;; ================= LIMPIEZA DE ACENTOS LATEX =================

(defun org-reading-stats-clean-latex-accents (str)
  "Elimina secuencias de escape de LaTeX comunes para acentos (como \\'e, \\`a, \\^o, etc.)."
  (if (or (null str) (string-empty-p str))
      ""
    (let ((clean str))
      (setq clean (replace-regexp-in-string "\\\\[`'^\"~=]\\([a-zA-Z]\\)" "\\1" clean))
      (setq clean (replace-regexp-in-string "{\\\\[`'^\"~=]\\([a-zA-Z]\\)}" "\\1" clean))
      clean)))

;;; ================= LÓGICA DE SINÓNIMOS CON PREVENCIÓN DE CASE-SENSITIVITY =================

(defun org-reading-stats-load-synonyms ()
  "Carga el archivo de sinónimos en minúsculas para una comparación case-insensitive limpia."
  (let ((synonyms '()))
    (when (file-exists-p org-reading-stats-author-synonyms-file)
      (with-temp-buffer
        (insert-file-contents org-reading-stats-author-synonyms-file)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (string-trim (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
            (when (and (not (string-empty-p line)) (string-match "^\\([^\t]+\\)\t\\([^\t]+\\)$" line))
              (let ((real-author (string-trim (match-string 1 line)))
                    ;; Guardamos la llave del sinónimo completamente en minúsculas para emparejar sin importar el case
                    (synonym-author (downcase (string-trim (match-string 2 line)))))
                (push (cons synonym-author real-author) synonyms))))
          (forward-line 1))))
    synonyms))

(defun capitalize-word-at-string (str)
  "Convierte un string a formato Capitalize respetando de forma segura guiones e iniciales."
  (if (or (null str) (string-empty-p str))
      ""
    (with-temp-buffer
      (insert (downcase str))
      (goto-char (point-min))
      ;; Capitaliza el inicio del texto y cualquier letra que siga a un espacio, guion o punto (ej: Solis-Lemus, P. R.)
      (while (re-search-forward "\\(?:^\\|[- \t.\"]+\\)\\([a-zñáéíóúü]\\)" nil t)
        (replace-match (upcase (match-string 1)) t t nil 1))
      (buffer-string))))

(defun org-reading-stats-normalize-author (author-str synonyms-alist)
  "Normaliza la cadena de autores aislando componentes, aplicando sinónimos o capitalizando de forma limpia."
  (if (or (null author-str) (string-empty-p author-str)) ;; Nota: Corregido bug potencial de str/author-str del original
      ""
    (let* (;; 1. Primero limpiamos acentos LaTeX comunes
           (author-clean (org-reading-stats-clean-latex-accents author-str))
           ;; 2. Quitamos llaves de protección de BibTeX descabelladas como {Rosemary} o \Relax
           (author-clean (replace-regexp-in-string "[{}]" "" author-clean))
           (author-clean (replace-regexp-in-string "\\\\Relax" "" author-clean))
           ;; 3. Separamos por 'and' ignorando mayúsculas/minúsculas y espacios/saltos de línea
           (individual-authors (split-string author-clean "[ \t\n\r]+[aA][nN][dD][ \t\n\r]+"))
           (processed-authors '()))
      (dolist (auth individual-authors)
        (let* (;; Limpiamos espacios internos dobles, saltos de línea y tabuladores del autor individual
               (clean-auth (replace-regexp-in-string "[ \t\n\r]+" " " auth))
               (clean-auth (string-trim clean-auth))
               ;; Buscamos en el alist usando la versión downcase estricta
               (match (assoc (downcase clean-auth) synonyms-alist)))
          (if match
              ;; Si hay sinónimo en tu txt, usamos el nombre real EXACTO que definiste ahí
              (push (cdr match) processed-authors)
            ;; Si NO hay sinónimo, aplicamos la nueva capitalización inteligente para unificar GRANT -> Grant
            (push (capitalize-word-at-string clean-auth) processed-authors))))
      ;; Volvemos a unir los autores con el estándar de BibTeX " and "
      (string-join (reverse processed-authors) " and "))))

;;; ================= PARSER NATIVO Y SEGURO DE ENTRADAS BIBTEX =================

(defun org-reading-stats-get-raw-field-bounds (end-entry field)
  "Busca un FIELD de manera case-insensitive y devuelve un cons (start . end) con su contenido crudo."
  (save-excursion
    (let ((case-fold-search t)
          (found nil))
      (while (and (not found) (re-search-forward (concat "\\b" field "[ \t]*=") end-entry t))
        (skip-chars-forward " \t\n\r")
        (let ((delim (char-after)))
          (cond
           ((eq delim ?{)
            (forward-char 1)
            (let ((start (point)))
              (backward-char 1)
              (ignore-errors
                (forward-sexp 1)
                (setq found (cons start (1- (point)))))))
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
          (if (re-search-forward (concat "@[a-zA-Z]+{[ \t]*" (regexp-quote clean-key) "[ \t]*,") nil t)
              (let* ((end-entry (save-excursion (if (re-search-forward "^}" nil t) (point) (point-max))))
                     (is-book (save-excursion (goto-char (line-beginning-position)) (looking-at-p "@book")))
                     (title "")
                     (author "")
                     (pages (if is-book 250 15)))
                
                (let ((title-bounds (org-reading-stats-get-raw-field-bounds end-entry "title")))
                  (when title-bounds
                    (setq title (buffer-substring-no-properties (car title-bounds) (cdr title-bounds)))))
                
                (let ((author-bounds (org-reading-stats-get-raw-field-bounds end-entry "author")))
                  (when author-bounds
                    (setq author (buffer-substring-no-properties (car author-bounds) (cdr author-bounds)))))
                
                (let ((pages-bounds (org-reading-stats-get-raw-field-bounds end-entry "pages")))
                  (when pages-bounds
                    (let ((pstr (buffer-substring-no-properties (car pages-bounds) (cdr pages-bounds))))
                      (cond
                       ((string-match "\\([0-9]+\\)[- ]+\\([0-9]+\\)" pstr)
                        (setq pages (1+ (abs (- (string-to-number (match-string 2 pstr))
                                              (string-to-number (match-string 1 pstr)))))))
                       ((string-match "^[ \t]*\\([0-9]+\\)[ \t]*$" pstr)
                        (setq pages (string-to-number (match-string 1 pstr))))))))
                
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
  "Genera JSON resolviendo definitivamente duplicidades por mayúsculas y aplicando sinónimos case-insensitive."
  (interactive)
  ;; MODIFICADO: Ahora apunta estrictamente a la raíz del paquete usando la constante inmutable
  (let* ((web-dir (expand-file-name "web/" org-reading-stats--package-root))
         (synonyms-alist (org-reading-stats-load-synonyms))
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
          
          ;; 1. Limpieza de acentos LaTeX e inyección de diccionario robusto
          (setq title-val (org-reading-stats-clean-latex-accents title-val))
          (setq author-val (org-reading-stats-normalize-author author-val synonyms-alist))
          
          ;; 2. Aplanamiento y limpieza sintáctica final para el título en el JSON
          (setq title-val (replace-regexp-in-string "[{}]" "" title-val))
          (setq title-val (replace-regexp-in-string "[ \t\n\r]+" " " title-val))
          (setq title-val (string-trim title-val))

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
    (message "Dashboard actualizado con éxito. Mapeo y tipografía unificados." (length results))))

(defun org-reading-stats-start ()
  "Start the local server and open the reading stats index."
  (interactive)
  (org-reading-stats-generate-json)
  ;; MODIFICADO: También se simplifica el servidor usando la constante de paquete limpia
  (let ((web-path (expand-file-name "web/" org-reading-stats--package-root)))
    (setq httpd-port 8087 
          httpd-root web-path)
    (httpd-start)
    (browse-url "http://localhost:8087/index.html")))

(provide 'org-reading-stats)
