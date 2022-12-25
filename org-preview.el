;; -*- lexical-binding: t; -*-
(require 'org)

(defun org-preview-format-latex
    (prefix &optional beg end dir overlays msg forbuffer processing-type)
  "Replace LaTeX fragments with links to an image.

The function takes care of creating the replacement image.

Only consider fragments between BEG and END when those are
provided.

When optional argument OVERLAYS is non-nil, display the image on
top of the fragment instead of replacing it.

PROCESSING-TYPE is the conversion method to use, as a symbol.

Some of the options can be changed using the variable
`org-format-latex-options', which see."
  (when (and overlays (fboundp 'clear-image-cache)) (clear-image-cache))
  (unless (eq processing-type 'verbatim)
    (let* ((math-regexp "\\$\\|\\\\[([]\\|^[ \t]*\\\\begin{[A-Za-z0-9*]+}")
	   (cnt 0)
	   checkdir-flag)
      (goto-char (or beg (point-min)))
      ;; Optimize overlay creation: (info "(elisp) Managing Overlays").
      (when (and overlays (memq processing-type '(dvipng imagemagick)))
	(overlay-recenter (or end (point-max))))
      (cond
       ((eq processing-type 'mathjax)
	;; Prepare for MathJax processing.
        (while (re-search-forward math-regexp end t)
	  (unless (and overlays
		       (eq (get-char-property (point) 'org-overlay-type)
			   'org-latex-overlay))
	    (let* ((context (org-element-context))
		   (type (org-element-type context)))
	      (when (memq type '(latex-environment latex-fragment))
	        (let ((block-type (eq type 'latex-environment))
		      (value (org-element-property :value context))
		      (beg (org-element-property :begin context))
		      (end (save-excursion
			     (goto-char (org-element-property :end context))
			     (skip-chars-backward " \r\t\n")
			     (point))))
                  (if (not (string-match "\\`\\$\\$?" value))
	              (goto-char end)
	            (delete-region beg end)
	            (if (string= (match-string 0 value) "$$")
	                (insert "\\[" (substring value 2 -2) "\\]")
	              (insert "\\(" (substring value 1 -1) "\\)")))))))))
       ((eq processing-type 'html)
        (while (re-search-forward math-regexp end t)
	  (unless (and overlays
		       (eq (get-char-property (point) 'org-overlay-type)
			   'org-latex-overlay))
	    (let* ((context (org-element-context))
		   (type (org-element-type context)))
	      (when (memq type '(latex-environment latex-fragment))
	        (let ((block-type (eq type 'latex-environment))
		      (value (org-element-property :value context))
		      (beg (org-element-property :begin context))
		      (end (save-excursion
			     (goto-char (org-element-property :end context))
			     (skip-chars-backward " \r\t\n")
			     (point))))
                  (goto-char beg)
	          (delete-region beg end)
	          (insert (org-format-latex-as-html value))))))))
       ((eq processing-type 'mathml)
	(while (re-search-forward math-regexp end t)
	  (unless (and overlays
		       (eq (get-char-property (point) 'org-overlay-type)
			   'org-latex-overlay))
	    (let* ((context (org-element-context))
		   (type (org-element-type context)))
	      (when (memq type '(latex-environment latex-fragment))
	        (let ((block-type (eq type 'latex-environment))
		      (value (org-element-property :value context))
		      (beg (org-element-property :begin context))
		      (end (save-excursion
			     (goto-char (org-element-property :end context))
			     (skip-chars-backward " \r\t\n")
			     (point))))
                  ;; Process to MathML.
		  (unless (org-format-latex-mathml-available-p)
		    (user-error "LaTeX to MathML converter not configured"))
		  (cl-incf cnt)
		  (when msg (message msg cnt))
		  (goto-char beg)
		  (delete-region beg end)
		  (insert (org-format-latex-as-mathml
			   value block-type prefix dir))))))))
       ((assq processing-type org-preview-latex-process-alist)
        (let* ((processing-info
                (cdr (assq processing-type org-preview-latex-process-alist)))
               (face (face-at-point))
               (fg
		(let ((color (plist-get org-format-latex-options
					:foreground)))
                  (if forbuffer
                      (cond
                       ((eq color 'auto)
                        (face-attribute face :foreground nil 'default))
                       ((eq color 'default)
                        (face-attribute 'default :foreground nil))
                       (t color))
                    color)))
               (bg
		(let ((color (plist-get org-format-latex-options
					:background)))
                  (if forbuffer
                      (cond
                       ((eq color 'auto)
                        (face-attribute face :background nil 'default))
                       ((eq color 'default)
                        (face-attribute 'default :background nil))
                       (t color))
                    color)))
               (imagetype (or (plist-get processing-info :image-output-type) "png"))
	       (absprefix (expand-file-name prefix dir))
               (options
		(org-combine-plists
		 org-format-latex-options
		 `(:foreground ,fg :background ,bg)))
               (math-text)
               (math-locations)
               (math-hashes))
          
          (unless checkdir-flag ; Ensure the directory exists.
	    (setq checkdir-flag t)
	    (let ((todir (file-name-directory absprefix)))
	      (unless (file-directory-p todir)
		(make-directory todir t))))

          (save-excursion
            (while (re-search-forward math-regexp end t)
              (unless (and overlays
                           (eq (get-char-property (point) 'org-overlay-type)
                               'org-latex-overlay))
                (let* ((context (org-element-context))
                       (type (org-element-type context)))
                  (when (memq type '(latex-environment latex-fragment))
                    (let* ((block-type (eq type 'latex-environment))
                           (value (org-element-property :value context))
                           (block-beg (org-element-property :begin context))
                           (block-end (save-excursion
                                        (goto-char (org-element-property :end context))
                                        (skip-chars-backward " \r\t\n")
                                        (point)))
                           (hash (sha1 (prin1-to-string
				        (list org-format-latex-header
					      org-latex-default-packages-alist
					      org-latex-packages-alist
					      org-format-latex-options
					      forbuffer value fg bg)))))
                      (push value math-text)
                      (push (cons block-beg block-end) math-locations)
                      (push hash math-hashes)))))))

          (pcase-let ((`(,texfilebase ,tex-process ,image-process)
                       (org-preview-create-formula-image
                        (mapconcat #'identity (nreverse math-text) "\n\n")
                        options forbuffer processing-type)))
            (set-process-sentinel
             image-process
             (lambda (proc signal)
               (when (string= signal "finished\n")
                 (let ((images (file-expand-wildcards
                                (concat texfilebase "*." imagetype)
                                'full)))
                   (print images (get-buffer "*scratch*"))
                   (cl-loop with loc = (point)
                            for hash in (nreverse math-hashes)
                            for (block-beg . block-end) in (nreverse math-locations)
                            for image-file in images
                            for movefile = (format "%s_%s.%s" absprefix hash imagetype)
                            do (copy-file image-file movefile 'replace)
                            do (if overlays
		                   (progn
		                     (dolist (o (overlays-in block-beg block-end))
		                       (when (eq (overlay-get o 'org-overlay-type)
		        	                 'org-latex-overlay)
		                         (delete-overlay o)))
		                     (org--make-preview-overlay block-beg block-end movefile imagetype)
		                     (goto-char block-end))
		                 (delete-region block-beg block-end)
		                 (insert
		                  (org-add-props link
		                      (list 'org-latex-src
		        	            (replace-regexp-in-string "\"" "" value)
		        	            'org-latex-src-embed-type
		        	            (if block-type 'paragraph 'character)))))
                            finally do (goto-char loc))))
               (unless (process-live-p proc)
                 (mapc #'delete-file (file-expand-wildcards (concat texfilebase "*") 'full))))))))
       (t
	(error "Unknown conversion process %s for LaTeX fragments"
	       processing-type))))))

(defun org-preview-create-formula-image
    (string options buffer &optional processing-type)
  
  (let* ((processing-type (or processing-type
                              org-preview-latex-default-process))
         (processing-info
          (cdr (assq processing-type org-preview-latex-process-alist)))
         (programs (plist-get processing-info :programs))
         (error-message (or (plist-get processing-info :message) ""))
         (image-input-type (plist-get processing-info :image-input-type))
	 (image-output-type (plist-get processing-info :image-output-type))
	 (post-clean (or (plist-get processing-info :post-clean)
			 '(".dvi" ".xdv" ".pdf" ".tex" ".aux" ".log"
			   ".svg" ".png" ".jpg" ".jpeg" ".out")))
	 (latex-header
	  (or (plist-get processing-info :latex-header)
	      (org-latex-make-preamble
	       (org-export-get-environment (org-export-get-backend 'latex))
	       org-format-latex-header
	       'snippet)))
         (latex-compiler (plist-get processing-info :latex-compiler))
	 (tmpdir temporary-file-directory)
	 (texfilebase (make-temp-name
		       (expand-file-name "orgtex" tmpdir)))
	 (texfile (concat texfilebase ".tex"))
	 (image-size-adjust (or (plist-get processing-info :image-size-adjust)
				'(1.0 . 1.0)))
	 (scale (* (if buffer (car image-size-adjust) (cdr image-size-adjust))
		   (or (plist-get options (if buffer :scale :html-scale)) 1.0)))
	 (dpi (* scale (if buffer (org--get-display-dpi) 140.0)))
	 (fg (or (plist-get options (if buffer :foreground :html-foreground))
		 "Black"))
	 (bg (or (plist-get options (if buffer :background :html-background))
		 "Transparent"))
	 (image-converter
          (or (and (string= bg "Transparent")
                   (plist-get processing-info :transparent-image-converter))
              (plist-get processing-info :image-converter)))
         (log-buf (get-buffer-create "*Org Preview LaTeX Output*"))
	 (resize-mini-windows nil))
    
    (dolist (program programs)
      (org-check-external-command program error-message))
    (if (eq fg 'default)
	(setq fg (org-latex-color :foreground))
      (setq fg (org-latex-color-format fg)))
    (setq bg (cond
	      ((eq bg 'default) (org-latex-color :background))
	      ((string= bg "Transparent") nil)
	      (t (org-latex-color-format bg))))
    ;; Remove TeX \par at end of snippet to avoid trailing space.
    (if (string-suffix-p string "\n")
        (aset string (1- (length string)) ?%)
      (setq string (concat string "%")))

    (with-temp-file texfile
      (insert latex-header)
      (insert "\n\\begin{document}\n"
	      "\\definecolor{fg}{rgb}{" fg "}%\n"
	      (if bg
		  (concat "\\definecolor{bg}{rgb}{" bg "}%\n"
			  "\n\\pagecolor{bg}%\n")
		"")
	      "\n{\\color{fg}\n"
	      string
	      "\n}\n"
	      "\n\\end{document}\n"))

    (let* ((latex-compiler
            (car '("latex -interaction nonstopmode -output-directory %o")))
           ;; (image-converter (car '("dvipng --follow -D %D -T tight %O")))
           (image-converter (car '("dvipng --follow -D %D -T tight -o %B-%%09d.png %O")))
           (tex-process)
           (conversion-process)
           (base-name (file-name-base texfile))
           (out-dir (or (file-name-directory texfile) "./"))
           (spec `((?D . ,(shell-quote-argument (format "%s" dpi)))
	           (?S . ,(shell-quote-argument (format "%s" (/ dpi 140.0))))
                   (?b . ,(shell-quote-argument base-name))
                   (?B . ,(shell-quote-argument texfilebase))
		   (?f . ,(shell-quote-argument texfile))
		   (?F . ,(shell-quote-argument (file-truename texfile)))
		   (?o . ,(shell-quote-argument out-dir))
		   (?O . ,(shell-quote-argument (expand-file-name
                                                 (concat base-name "." image-input-type) out-dir)))
                   )))
      (setq tex-process
            (make-process :name (format "Org-Preview-%s" (file-name-base texfile))
                          :buffer log-buf
                          :command (split-string-shell-command (format-spec latex-compiler spec))
                          :sentinel (lambda (proc signal)
                                      (unless (process-live-p proc)
                                        (dolist (e (cl-remove (concat "." image-input-type) post-clean))
                                          (when (file-exists-p (concat texfilebase e))
                                            (delete-file (concat texfilebase e))))))))
      (process-send-string tex-process
                           (format-spec
                            "\\PassOptionsToPackage{active,tightpage,auctex}{preview}\\AtBeginDocument{\\ifx\\ifPreview\\undefined\\RequirePackage[displaymath,floats,graphics,textmath,sections,footnotes]{preview}[2004/11/05]\\fi}\\input\\detokenize{%f}\n" spec))
      (setq image-process
            (make-process :name (format "Org-Convert-%s-%s"
                                        (file-name-base texfile)
                                        (symbol-name processing-type))
                          :buffer (format "Org-Convert-%s-%s.out"
                                          (file-name-base texfile)
                                          (symbol-name processing-type))
                          :command (split-string-shell-command (format-spec image-converter spec))))
      (list texfilebase tex-process image-process))))


;; "pdflatex  -file-line-error   --synctex=1 \"\\nonstopmode\\nofiles\\PassOptionsToPackage{active,tightpage,auctex}{preview}\\AtBeginDocument{\\ifx\\ifPreview\\undefined\\RequirePackage[displaymath,floats,graphics,textmath,sections,footnotes]{preview}[2004/11/05]\\fi}\" \"\\input\" \"\\detokenize{\" _region_.tex \"}\""

(define-minor-mode org-preview-mode
  "Asynchronous and batched (much, much faster) LaTeX previews for Org-mode."
  :global nil
  :version "0.10"
  :lighter "[preview]"
  :group 'org
  (if org-preview-mode
      ;; Turning the mode on
      (advice-add 'org-format-latex :override #'org-preview-format-latex)
    ;; Turning the mode off
    (advice-remove 'org-format-latex #'org-preview-format-latex)))
