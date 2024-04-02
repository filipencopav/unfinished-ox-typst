;;;; UTIL VARS
(defconst org-typst--special-characters
  "();[]#*`_<>@$\\/"
  "User-written characters in the org mode document that need to be escaped to not interfere with typst syntax.

For `/' and `*' we need additional checks to see if it's a comment or not.")

;;;; UTIL FUNCTIONS
(defun org-typst--escape-content-string (string)
  "Escapes a content string. If, for example, there is a `]' character in the string, then it is escaped so that it doesn't act as a closing delimeter for a content block opened by ox-typst."
  (mapconcat
   (lambda (char)
     (if (seq-contains-p org-typst--special-characters char)
         (format "\\%c" char)
       (string char)))
   string))

(defun org-typst--escape-raw-string (string)
  (mapconcat
   (lambda (char)
     (if (memq char '(?\" ?\\))
         (format "\\%c" char)
       (string char)))
   string))

(defun org-typst--reference (datum info &optional named-only)
  "Return an appropriate reference for DATUM.

DATUM is an element or a `target' type object.  INFO is the
current export state, as a plist.

When NAMED-ONLY is non-nil and DATUM has no NAME keyword, return
nil.  This doesn't apply to headlines, inline tasks, radio
targets and targets.

Taken from ox-html.el, copied into ox-typst.el since it's an ox-html
internal, just in case it gets deleted"
  (let* ((type (org-element-type datum))
         (user-label
          (org-element-property
           (pcase type
             ((or `headline `inlinetask) :CUSTOM_ID)
             ((or `radio-target `target) :value)
             (_ :name))
           datum)))
    (cond
     ((and user-label
           (or (plist-get info :html-prefer-user-labels)
               ;; Used CUSTOM_ID property unconditionally.
               (memq type '(headline inlinetask))))
      user-label)
     ((and named-only
           (not (memq type '(headline inlinetask radio-target target)))
           (not user-label))
      nil)
     (t
      (org-export-get-reference datum info)))))


;;;; BACKEND FUNCTIONS
(defun org-typst-radio-target (radio-target contents info)
  (format "#label(\"%s\");%s" (org-typst--reference radio-target info) contents))

(defun org-typst-link (link contents info)
  ;; https://typst.app//docs/reference/model/link
  ;; > To link to web pages, dest should be a valid URL string. If the URL is in the mailto: or tel: scheme and the body parameter is omitted, the email address or phone number will be the link's body, without the scheme.
  ;; For ex. `#link("mailto:thisperson@gmail.com")' will produce `thisperson@gmail.com' (clickable link)
  ;; Therefore mailto: links can map 1:1 with org mode mailto: links' behavior/look
  (let ((contents (org-string-nw-p contents))
        (type (org-element-property :type link))
        (path (org-element-property :path link)))
    (pcase (intern type)
      ('radio
       (let ((destination (org-export-resolve-radio-link link info)))
         (if destination
	     (format "#link(label(\"%s\"), \"%s\");"
		     (org-typst--reference destination info)
		     contents)
           (org-string-nw-p contents))))
      ((or 'mailto 'http 'https 'ftp 'news)
       (format "#link(\"%s\", [%s])"
               (url-encode-url (concat type ":" (org-element-property :raw-link link)))
               (or contents
                   (->> link
                        (org-element-property :raw-link)
                        org-typst--escape-content-string))))
      ('fuzzy
       (let ((ref (-> (org-export-resolve-fuzzy-link link info)
                      (org-typst--reference info))))
         (format "#link(label(\"%s\"), \"%s\")"
                 ref
                 (or contents
                     (org-element-property :path link)))))
      (_
       ;; TODO: support other link types
       (warn "Unsupported link type `%s'" type)
       nil))))

(defun org-typst-template (ready-file-contents export-options)
  ;; TODO: Source template configs from `export-options'
  (concat
   "#set heading(numbering: \"1. \");\n"
   ready-file-contents))

(defun org-typst-plain-text (plain-text info)
  (org-typst--escape-content-string plain-text))

(defun org-typst-section (section contents info)
  contents)

(defun org-typst-paragraph (paragraph contents info)
  contents)

(defun org-typst-bold (bold contents info)
  (format "#strong[%s];" contents))

(defun org-typst-code (code contents info)
  (format "#raw(\"%s\");"
          (->> code
               (org-element-property :value)
               org-typst--escape-raw-string)))

(defun org-typst-entity (entity contents info)
  (org-element-property :utf-8 entity))

(defun org-typst-export-snippet (export-snippet contents info)
  (when (equal "typst" (org-element-property :back-end export-snippet))
    (org-element-property :value export-snippet)))

(defun org-typst-footnote-reference (footnote-reference contents info)
  (let* ((definition (org-export-get-footnote-definition footnote-reference info))
         (definition-ref (org-typst--reference definition info)))
    (cond
     ((eq 'inline (org-element-property :type footnote-reference))
      ;; if it's inline then the CAR of the definition will be a string
      (format "#footnote[%s];" (car definition)))

     ((org-export-footnote-first-reference-p footnote-reference info)
      (format "#footnote[%s];#label(\"%s\");"
              (org-export-data definition info)
              definition-ref))

     (t
      (format "#footnote(label(\"%s\"))" definition-ref)))))

(defun org-typst-inline-src-block (inline-src-block contents info)
  (format "#raw(\"%s\", block: false, lang: \"%s\");"
          (org-typst--escape-raw-string
           (org-element-property :value inline-src-block))
          (let ((org-lang (org-element-property :language inline-src-block)))
            (or (alist-get (intern org-lang) (plist-get info :typst-langs))
                (downcase org-lang)))))

(defun org-typst-italic (italic contents info)
  (format "#emph[%s];" contents))

(defun org-typst-line-break (line-break contents info)
  (format "\\\n" contents))

(defun org-typst-headline (headline contents info)
  ;; TODO: Implement all the optional arguments listed in the typst docs
  ;; https://typst.app/docs/reference/model/heading/
  (let ((level (org-element-property :level headline))
        (raw-value (org-element-property :raw-value headline)))
    (format "#heading(level: %d, \"%s\");#label(\"%s\");\n%s"
            level
            raw-value
            (org-export-get-reference headline info)
            (or contents ""))))

;; org-export-define-backend
(org-export-define-backend 'typst
  '((template . org-typst-template)
    (plain-text . org-typst-plain-text)
    (section . org-typst-section)
    (paragraph . org-typst-paragraph)
    (bold . org-typst-bold)
    (code . org-typst-code)
    (entity . org-typst-entity)
    (export-snippet . org-typst-export-snippet)
    (footnote-reference . org-typst-footnote-reference)
    (inline-src-block . org-typst-inline-src-block)
    (italic . org-typst-italic)
    (line-break . org-typst-line-break)
    (radio-target . org-typst-radio-target)
    (link . org-typst-link)
    (headline . org-typst-headline)

    ;; (center-block . org-latex-center-block)
    ;; (clock . org-latex-clock)
    ;; (drawer . org-latex-drawer)
    ;; (dynamic-block . org-latex-dynamic-block)
    ;; (example-block . org-latex-example-block)
    ;; (export-block . org-latex-export-block)
    ;; (fixed-width . org-latex-fixed-width)
    ;; (footnote-definition . org-latex-footnote-definition)
    ;; (horizontal-rule . org-latex-horizontal-rule)
    ;; (inlinetask . org-latex-inlinetask)
    ;; (item . org-latex-item)
    ;; (keyword . org-latex-keyword)
    ;; (latex-environment . org-latex-latex-environment)
    ;; (latex-fragment . org-latex-latex-fragment)
    ;; (node-property . org-latex-node-property)
    ;; (plain-list . org-latex-plain-list)
    ;; (planning . org-latex-planning)
    ;; (property-drawer . org-latex-property-drawer)
    ;; (quote-block . org-latex-quote-block)
    ;; (special-block . org-latex-special-block)
    ;; (src-block . org-latex-src-block)
    ;; (statistics-cookie . org-latex-statistics-cookie)
    ;; (strike-through . org-latex-strike-through)
    ;; (subscript . org-latex-subscript)
    ;; (superscript . org-latex-superscript)
    ;; (table . org-latex-table)
    ;; (table-cell . org-latex-table-cell)
    ;; (table-row . org-latex-table-row)
    ;; (target . org-latex-target)
    ;; (timestamp . org-latex-timestamp)
    ;; (underline . org-latex-underline)
    ;; (verbatim . org-latex-verbatim)
    ;; (verse-block . org-latex-verse-block)
    ;; ;; Pseudo objects and elements.
    ;; (latex-math-block . org-latex-math-block)
    ;; (latex-matrices . org-latex-matrices)
    )
  :options-alist
  '((:typst-langs "TYPST_LANGS" parse '((emacs-lisp . "elisp"))))
  )

;; Devel util functions
(defun org-typst--export-buffer ()
  (interactive)
  (org-export-to-buffer 'typst "*Org Typst Export*"))

(defun org-typst--export-file ()
  (interactive)
  (let* ((bufname (buffer-name (current-buffer)))
         (extension-start (string-match "\\.org$" bufname 0 t))
         (filename-no-ext (when extension-start (substring bufname 0 extension-start))))
    (when (and filename-no-ext (> (length filename-no-ext) 0))
      (let ((new-filename (concat filename-no-ext ".typ")))
        (org-export-to-file 'typst new-filename)))))

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
