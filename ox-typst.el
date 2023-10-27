;; org-export-define-backend

(defun org-typst--escape-double-quote-in-string (string)
  (apply
   'concat
   (seq-map
    (lambda (char)
      (if (memq char '(?\" ?\\))
          (format "\\%c" char)
        (string char)))
    string)))

(defun org-typst-link (link contents info)
  (pcase (intern (org-element-property :type link))
    ('radio
     (let ((destination (org-export-resolve-radio-link link info)))
	     (if (not destination) (org-string-nw-p contents)
	       (format "#link(label(\"%s\"), %s);"
		             (org-export-get-reference destination info)
		             contents))))
    (_
     (concat
      "#link(\""
      (org-element-property :raw-link link)
      "\")["
      contents
      "];"))))

(org-export-define-backend 'typst
  '((template
     . (lambda (ready-file-contents export-options)
         ready-file-contents))
    (plain-text
     . (lambda (plain-text info)
         plain-text))
    (section
     . (lambda (section contents info)
         contents))
    (paragraph
     . (lambda (paragraph contents info)
         contents))
    (bold
     . (lambda (bold contents info)
         (format "#strong[%s];" contents)))
    (code
     . (lambda (code contents info)
         (format "#raw[%s];" contents)))
    (entity
     . (lambda (entity contents info)
         (org-element-property :utf-8 entity)))
    (export-snippet
     . (lambda (export-snippet contents info)
         (when (equal "typst" (org-element-property :back-end export-snippet))
           (org-element-property :value export-snippet))))
    (footnote-reference
     . (lambda (footnote-reference contents info)
         (format " #footnote[%s];"
                 (car (org-export-get-footnote-definition footnote-reference info)))))
    (inline-src-block
     . (lambda (inline-src-block contents info)
         (format "#raw(\"%s\", block: false, lang: \"%s\");"
                 (org-typst--escape-double-quote-in-string
                  (org-element-property :value inline-src-block))
                 (let ((org-lang (org-element-property :language inline-src-block)))
                   ;; Maybe find an appropriate name in the :typst-langs plist
                   (or (alist-get (intern org-lang) (plist-get info :typst-langs))
			                 (downcase org-lang))))))
    (italic
     . (lambda (italic contents info)
         (format "#emph[%s];" contents)))
    (line-break
     . (lambda (line-break contents info)
         (format "\ " contents)))
    (link . org-typst-link)
    (macro
     . (lambda (macro contents info)
         (format "fml")))
    (radio-target
     . (lambda (radio-target contents info)
         (format "%s" contents)))
    )
  :options-alist
  '((:typst-langs "TYPST_LANGS" parse '((emacs-lisp . "elisp"))))
  )

(defun org-typst-export-as-typst-buffer ()
  (interactive)
  (org-export-to-buffer 'typst "*Org Typst Export*"))

(defvar objects
  '(bold code entity export-snippet footnote-reference inline-babel-call inline-src-block italic line-break latex-fragment link macro radio-target statistics-cookie strike-through subscript superscript table-cell target timestamp underline verbatim))

(defvar elements
  '(babel-call center-block clock comment comment-block diary-sexp drawer dynamic-block example-block export-block fixed-width footnote-definition headline horizontal-rule inlinetask item keyword latex-environment node-property paragraph plain-list planning property-drawer quote-block section special-block src-block table table-row verse-block))

(let* ((minimal-set '(bold code entity italic latex-fragment strike-through
			     subscript superscript underline verbatim))
	 (standard-set
	  (remq 'citation-reference (remq 'table-cell org-element-all-objects)))
	 (standard-set-no-line-break (remq 'line-break standard-set)))
    `((bold ,@standard-set)
      (citation citation-reference)
      (citation-reference ,@minimal-set)
      (footnote-reference ,@standard-set)
      (headline ,@standard-set-no-line-break)
      (inlinetask ,@standard-set-no-line-break)
      (italic ,@standard-set)
      (item ,@standard-set-no-line-break)
      (keyword ,@(remq 'footnote-reference standard-set))
      ;; Ignore all links in a link description. Also ignore
      ;; radio-targets and line breaks.
      (link export-snippet inline-babel-call inline-src-block macro
	    statistics-cookie ,@minimal-set)
      (paragraph ,@standard-set)
      ;; Remove any variable object from radio target as it would
      ;; prevent it from being properly recognized.
      (radio-target ,@minimal-set)
      (strike-through ,@standard-set)
      (subscript ,@standard-set)
      (superscript ,@standard-set)
      ;; Ignore inline babel call and inline source block as formulas
      ;; are possible. Also ignore line breaks and statistics
      ;; cookies.
      (table-cell citation export-snippet footnote-reference link macro
                  radio-target target timestamp ,@minimal-set)
      (table-row table-cell)
      (underline ,@standard-set)
      (verse-block ,@standard-set)))

(defvar objects-unused-in-ox-html
  '(citation citation-reference inline-babel-call macro))

(defvar elements-unused-in-ox-html
  '(babel-call comment comment-block diary-sexp footnote-definition))
