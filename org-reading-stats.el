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
