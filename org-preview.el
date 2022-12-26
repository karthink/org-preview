;;; org-preview.el --- Fast, async LaTeX previews for Org  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Keywords: tex, extensions, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; To use this package, turn on `org-preview-mode'.

;;; Code:

(require 'org)

(defvar org-preview--debug-msg t)
(defvar org-preview--log-buf "*Org Preview Log*")

(defsubst org-preview-report (msg start-time)
  (when org-preview--debug-msg 
    (with-current-buffer (get-buffer-create org-preview--log-buf)
      (insert (format "%.4f:    " (time-to-seconds (time-since start-time)))
              msg "\n"))))

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
       ((eq processing-type 'imagemagick)
        (user-error "Imagemagick based previews are currently not supported.\nPlease customize `org-preview-latex-default-process'."))
       ((assq processing-type org-preview-latex-process-alist)
        (let* ((processing-info
                (cdr (assq processing-type org-preview-latex-process-alist)))
               (face (face-at-point))
               (start-time (current-time))
               (num-overlays)
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
               (image-output-type (or (plist-get processing-info :image-output-type) "png"))
               (image-input-type (or (plist-get processing-info :image-input-type) "dvi"))
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

          (setq num-overlays (length math-locations))
          
          (pcase-let ((`(,texfilebase ,tex-process ,image-process)
                       (org-preview-create-formula-image
                        (mapconcat #'identity (nreverse math-text) "\n\n")
                        options forbuffer processing-type start-time)))
            (set-process-sentinel
             image-process
             (lambda (proc signal)
               (when org-preview--debug-msg
                 (unless (process-live-p proc)
                   (org-preview-report "DVI processing" start-time)))
               (when (string= signal "finished\n")
                 (let ((images (file-expand-wildcards
                                (concat texfilebase "*." image-output-type)
                                'full)))
                   (cl-loop with loc = (point)
                            for hash in (nreverse math-hashes)
                            for (block-beg . block-end) in (nreverse math-locations)
                            for image-file in images
                            for movefile = (format "%s_%s.%s" absprefix hash image-output-type)
                            do (copy-file image-file movefile 'replace)
                            do (if overlays
		                   (progn
		                     (dolist (o (overlays-in block-beg block-end))
		                       (when (eq (overlay-get o 'org-overlay-type)
		        	                 'org-latex-overlay)
		                         (delete-overlay o)))
		                     (org--make-preview-overlay block-beg block-end movefile image-output-type)
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
                 (mapc #'delete-file (file-expand-wildcards (concat texfilebase "*." image-output-type) 'full))
                 (delete-file (concat texfilebase "." image-input-type)))
               (when org-preview--debug-msg
                 (org-preview-report "Overlay placement" start-time)
                 (with-current-buffer org-preview--log-buf
                   (insert (format "Previews: %d, Process: %S\n\n"
                                   num-overlays processing-type)))))))))
       (t
	(error "Unknown conversion process %s for LaTeX fragments"
	       processing-type))))))

(defun org-preview-create-formula-image
    (string options buffer &optional processing-type start-time)
  
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

    (let* (;; (latex-compiler
           ;;  (car '("latex -interaction nonstopmode -output-directory %o")))
           ;; (image-converter (car '("dvipng --follow -bg %g -fg %c -D %D -T tight -o %B-%%09d.png %O")))
           ;; (image-converter (car '("dvisvgm --page=1- -n -b min -c %S -o %B-%%9p.svg %O")))
           (tex-process)
           (image-process)
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
                   (?c . ,(shell-quote-argument (concat "rgb " (replace-regexp-in-string "," " " fg))))
                   (?g . ,(shell-quote-argument (concat "rgb " (replace-regexp-in-string "," " " bg)))))))
      (when org-preview--debug-msg
        (org-preview-report "Preprocessing" start-time))
      (setq tex-process
            (make-process :name (format "Org-Preview-%s" (file-name-base texfile))
                          :buffer log-buf
                          :command (split-string-shell-command (format-spec (car latex-compiler) spec))
                          :sentinel (lambda (proc signal)
                                      (unless (process-live-p proc)
                                        (org-preview-report "Tex process" start-time)
                                        (dolist (e (delete (concat "." image-input-type) post-clean))
                                          (when (file-exists-p (concat texfilebase e))
                                            (delete-file (concat texfilebase e))))
                                        (org-preview-report "Tex cleanup" start-time)))))
      (process-send-string tex-process
                           (format-spec
                            "\\PassOptionsToPackage{noconfig,active,tightpage,auctex}{preview}\\AtBeginDocument{\\ifx\\ifPreview\\undefined\\RequirePackage[displaymath,floats,graphics,textmath,sections,footnotes]{preview}[2004/11/05]\\fi}\\input\\detokenize{%f}\n"
                            spec))
      (when (equal processing-type 'dvisvgm)
        (while (process-live-p tex-process)
          (accept-process-output tex-process)))
      (setq image-process
            (make-process :name (format "Org-Convert-%s-%s"
                                        (file-name-base texfile)
                                        (symbol-name processing-type))
                          :buffer (format "*Org Convert %s %s*"
                                          (file-name-base texfile)
                                          (symbol-name processing-type))
                          :command (split-string-shell-command (format-spec (car image-converter) spec))))
      (list texfilebase tex-process image-process))))



;; Ignore the rest of this file. It's some glue to turn this feature into a
;; minor-mode without messing up the User's state.

(require 'map)
(defvar org-preview--dvipng-latex-compiler nil)
(defvar org-preview--dvipng-image-converter nil)
(defvar org-preview--dvipng-transparent-image-compiler nil)
(defvar org-preview--dvisvgm-latex-compiler nil)
(defvar org-preview--dvisvgm-image-converter nil)

(defsubst org-preview--get (&rest keys)
  (map-nested-elt org-preview-latex-process-alist keys))

(define-minor-mode org-preview-mode
  "Asynchronous and batched (much, much faster) LaTeX previews for Org-mode."
  :global t
  :version "0.10"
  :lighter nil
  :group 'org
  (if org-preview-mode
      ;; Turning the mode on
      (progn
        (setq org-preview--dvipng-latex-compiler
              (org-preview--get 'dvipng :latex-compiler))
        (setq org-preview--dvipng-image-converter
              (org-preview--get 'dvipng :image-converter))
        (setq org-preview--dvipng-transparent-image-compiler
              (org-preview--get 'dvipng :transparent-image-compiler))
        (setq org-preview--dvisvgm-latex-compiler
              (org-preview--get 'dvisvgm :latex-compiler))
        (setq org-preview--dvisvgm-image-converter
              (org-preview--get 'dvisvgm :image-converter))
        (let ((dvipng-proc (alist-get 'dvipng org-preview-latex-process-alist)))
          (setq
           dvipng-proc
           (plist-put dvipng-proc
                      :latex-compiler
                      '("latex -interaction nonstopmode -output-directory %o"))
           dvipng-proc
           (plist-put dvipng-proc
                      :image-converter
                      '("dvipng --follow -bg %g -fg %c -D %D -T tight -o %B-%%09d.png %O"))
           dvipng-proc
           (plist-put dvipng-proc
                      :transparent-image-converter
                      '("dvipng --follow -D %D -T tight -bg Transparent -fg %c -o %B-%%09d.png %O")))
          ;; (map-put! org-preview-latex-process-alist 'dvipng dvipng-proc)
          )
        (let ((dvisvgm-proc (alist-get 'dvisvgm org-preview-latex-process-alist)))
          (setq
           dvisvgm-proc
           (plist-put dvisvgm-proc
                      :latex-compiler
                      '("latex -interaction nonstopmode -output-directory %o"))
           dvisvgm-proc
           (plist-put dvisvgm-proc
                      :image-converter
                      '("dvisvgm --page=1- -n -b min -c %S -o %B-%%9p.svg %O"))))
          ;; (map-put! org-preview-latex-process-alist 'dvisvgm dvisvgm-proc)
          
          (advice-add 'org-format-latex :override #'org-preview-format-latex))
    ;; Turning the mode off
    
    (let ((dvipng-proc (alist-get 'dvipng org-preview-latex-process-alist)))
          (setq
           dvipng-proc
           (plist-put dvipng-proc :latex-compiler
                      org-preview--dvipng-latex-compiler)
           dvipng-proc
           (plist-put dvipng-proc :image-converter
                      org-preview--dvipng-image-converter)
           dvipng-proc
           (plist-put dvipng-proc :transparent-image-converter
                      org-preview--dvipng-transparent-image-compiler))
          ;; (map-put! org-preview-latex-process-alist 'dvipng dvipng-proc)
          )
    (let ((dvisvgm-proc (alist-get 'dvisvgm org-preview-latex-process-alist)))
      (setq
       dvisvgm-proc
       (plist-put dvisvgm-proc :latex-compiler
                  org-preview--dvisvgm-latex-compiler)
       dvisvgm-proc
       (plist-put dvisvgm-proc :image-converter
                  org-preview--dvisvgm-image-converter))
      ;; (map-put! org-preview-latex-process-alist 'dvisvgm dvisvgm-proc)
      )
    (advice-remove 'org-format-latex #'org-preview-format-latex)))

(provide 'org-preview)
;;; org-preview.el ends here
