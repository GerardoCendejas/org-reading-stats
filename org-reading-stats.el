(require 'simple-httpd)
(require 'json)

;; Configuración de ruta (Ajusta si es necesario)
(defvar org-reading-stats-file "~/.emacs.d/org/read_papers.org"
  "Ruta a tu archivo org de lecturas.")

(defun org-reading-stats-generate-json ()
  "Prueba de conexión: Captura cualquier línea que empiece con asteriscos."
  (let (results
        (web-dir (expand-file-name "web" (file-name-directory (or load-file-name (buffer-file-name) (buffer-file-name))))))
    (with-temp-buffer
      (insert-file-contents org-reading-stats-file)
      (goto-char (point-min))
      ;; Regex simplificado: cualquier línea que empiece con asteriscos y espacio
      (while (re-search-forward "^\\*+ +\\(.*\\)$" nil t)
        (let ((title (match-string 1)))
          (push `((cite . ,title)
                  (timestamp . "2026-02-19 12:00")) ;; Fecha falsa para probar
                results))))
    (with-temp-file (expand-file-name "data.json" web-dir)
      (insert (json-encode results)))
    (message "JSON de prueba generado con %d entradas." (length results))))

(defun org-reading-stats-start ()
  "Genera los datos e inicia el servidor web."
  (interactive)
  (org-reading-stats-generate-json)
  (httpd-stop)
  (setq httpd-port 8080)
  ;; La raíz del servidor será la carpeta 'web'
  (setq httpd-root (expand-file-name "web" (file-name-directory (or load-file-name (buffer-file-name) (buffer-file-name)))))
  (httpd-start)
  (message "Dashboard disponible en: http://localhost:8080/index.html"))

(provide 'org-reading-stats)
