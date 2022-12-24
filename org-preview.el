(require 'org)
(require 'seq)

(defun org-format-latex
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
		(cond
		 ((eq processing-type 'mathjax)
		  ;; Prepare for MathJax processing.
		  (if (not (string-match "\\`\\$\\$?" value))
		      (goto-char end)
		    (delete-region beg end)
		    (if (string= (match-string 0 value) "$$")
			(insert "\\[" (substring value 2 -2) "\\]")
		      (insert "\\(" (substring value 1 -1) "\\)"))))
		 ((eq processing-type 'html)
		  (goto-char beg)
		  (delete-region beg end)
		  (insert (org-format-latex-as-html value)))
		 ((assq processing-type org-preview-latex-process-alist)
		  ;; Process to an image.
		  (cl-incf cnt)
		  (goto-char beg)
		  (let* ((processing-info
			  (cdr (assq processing-type org-preview-latex-process-alist)))
			 (face (face-at-point))
			 ;; Get the colors from the face at point.
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
			 (hash (sha1 (prin1-to-string
				      (list org-format-latex-header
					    org-latex-default-packages-alist
					    org-latex-packages-alist
					    org-format-latex-options
					    forbuffer value fg bg))))
			 (imagetype (or (plist-get processing-info :image-output-type) "png"))
			 (absprefix (expand-file-name prefix dir))
			 (linkfile (format "%s_%s.%s" prefix hash imagetype))
			 (movefile (format "%s_%s.%s" absprefix hash imagetype))
			 (sep (and block-type "\n\n"))
			 (link (concat sep "[[file:" linkfile "]]" sep))
			 (options
			  (org-combine-plists
			   org-format-latex-options
			   `(:foreground ,fg :background ,bg))))
		    (when msg (message msg cnt))
		    (unless checkdir-flag ; Ensure the directory exists.
		      (setq checkdir-flag t)
		      (let ((todir (file-name-directory absprefix)))
			(unless (file-directory-p todir)
			  (make-directory todir t))))
		    (unless (file-exists-p movefile)
		      (org-create-formula-image
		       value movefile options forbuffer processing-type))
		    (if overlays
			(progn
			  (dolist (o (overlays-in beg end))
			    (when (eq (overlay-get o 'org-overlay-type)
				      'org-latex-overlay)
			      (delete-overlay o)))
			  (org--make-preview-overlay beg end movefile imagetype)
			  (goto-char end))
		      (delete-region beg end)
		      (insert
		       (org-add-props link
			   (list 'org-latex-src
				 (replace-regexp-in-string "\"" "" value)
				 'org-latex-src-embed-type
				 (if block-type 'paragraph 'character)))))))
		 ((eq processing-type 'mathml)
		  ;; Process to MathML.
		  (unless (org-format-latex-mathml-available-p)
		    (user-error "LaTeX to MathML converter not configured"))
		  (cl-incf cnt)
		  (when msg (message msg cnt))
		  (goto-char beg)
		  (delete-region beg end)
		  (insert (org-format-latex-as-mathml
			   value block-type prefix dir)))
		 (t
		  (error "Unknown conversion process %s for LaTeX fragments"
			 processing-type)))))))))))

(defun org-create-formula-image
    (string tofile options buffer &optional processing-type)
  "Create an image from LaTeX source using external processes.

The LaTeX STRING is saved to a temporary LaTeX file, then
converted to an image file by process PROCESSING-TYPE defined in
`org-preview-latex-process-alist'.  A nil value defaults to
`org-preview-latex-default-process'.

The generated image file is eventually moved to TOFILE.

The OPTIONS argument controls the size, foreground color and
background color of the generated image.

When BUFFER non-nil, this function is used for LaTeX previewing.
Otherwise, it is used to deal with LaTeX snippets showed in
a HTML file."
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
	 (resize-mini-windows nil)) ;Fix Emacs flicker when creating image.
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
    (let* ((err-msg (format "Please adjust `%s' part of \
`org-preview-latex-process-alist'."
			    processing-type))
	   (image-input-file
	    (org-compile-file
	     texfile latex-compiler image-input-type err-msg log-buf))
	   (image-output-file
	    (org-compile-file
	     image-input-file image-converter image-output-type err-msg log-buf
	     `((?D . ,(shell-quote-argument (format "%s" dpi)))
	       (?S . ,(shell-quote-argument (format "%s" (/ dpi 140.0))))))))
      (copy-file image-output-file tofile 'replace)
      (dolist (e post-clean)
	(when (file-exists-p (concat texfilebase e))
	  (delete-file (concat texfilebase e))))
      image-output-file)))

(defun org-create-formula-image
    (string tofile options buffer &optional processing-type)
  "Create an image from LaTeX source using external processes.

The LaTeX STRING is saved to a temporary LaTeX file, then
converted to an image file by process PROCESSING-TYPE defined in
`org-preview-latex-process-alist'.  A nil value defaults to
`org-preview-latex-default-process'.

The generated image file is eventually moved to TOFILE.

The OPTIONS argument controls the size, foreground color and
background color of the generated image.

When BUFFER non-nil, this function is used for LaTeX previewing.
Otherwise, it is used to deal with LaTeX snippets showed in
a HTML file."
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
	 (resize-mini-windows nil)) ;Fix Emacs flicker when creating image.
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
    (let* ((err-msg (format "Please adjust `%s' part of \
`org-preview-latex-process-alist'."
			    processing-type))
	   (image-input-file
	    (org-compile-file
	     texfile latex-compiler image-input-type err-msg log-buf))
	   (image-output-file
	    (org-compile-file
	     image-input-file image-converter image-output-type err-msg log-buf
	     `((?D . ,(shell-quote-argument (format "%s" dpi)))
	       (?S . ,(shell-quote-argument (format "%s" (/ dpi 140.0))))))))
      (copy-file image-output-file tofile 'replace)
      (dolist (e post-clean)
	(when (file-exists-p (concat texfilebase e))
	  (delete-file (concat texfilebase e))))
      image-output-file)))



"pdflatex  -file-line-error   --synctex=1 \"\\nonstopmode\\nofiles\\PassOptionsToPackage{active,tightpage,auctex}{preview}\\AtBeginDocument{\\ifx\\ifPreview\\undefined\\RequirePackage[displaymath,floats,graphics,textmath,sections,footnotes]{preview}[2004/11/05]\\fi}\" \"\\input\" \"\\detokenize{\" _region_.tex \"}\""
