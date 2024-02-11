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

(defun org-typst--reference (datum info &optional named-only)
  "Return an appropriate reference for DATUM.

DATUM is an element or a `target' type object.  INFO is the
current export state, as a plist.

When NAMED-ONLY is non-nil and DATUM has no NAME keyword, return
nil.  This doesn't apply to headlines, inline tasks, radio
targets and targets.

Taken from ox-html.el, copied here since it's an internal, just
in case it gets deleted"
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

(defun org-typst-radio-target (radio-target contents info)
  (format "#label(\"%s\");%s" (org-typst--reference radio-target info) contents))

(defun org-typst-link (link contents info)
  ;; https://typst.app//docs/reference/model/link
  ;; > To link to web pages, dest should be a valid URL string. If the URL is in the mailto: or tel: scheme and the body parameter is omitted, the email address or phone number will be the link's body, without the scheme.
  ;; For ex. `#link("mailto:thisperson@gmail.com")' will produce `thisperson@gmail.com' (clickable link)
  ;; Therefore mailto: links can map 1:1 with org mode mailto: links' behavior/look
  (pcase (intern (org-element-property :type link))
    ('radio
     (let ((destination (org-export-resolve-radio-link link info)))
       (if destination
	   (format "#link(label(\"%s\"), \"%s\");"
		   (org-typst--reference destination info)
		   contents)
         (org-string-nw-p contents))))
    ('mailto
     '(TODO: treat as mailto link))
    ((or 'http 'https 'ftp 'mailto 'news)
     (concat
      "#link(\""
      (org-element-property :raw-link link)
      "\")["
      contents
      "];"))
    (_
     ())))

(defun org-typst-template (ready-file-contents export-options)
  ready-file-contents)

;; TODO: Escape the characters: "();[]#*`_<>@$\\/**///"
(defun org-typst-plain-text (plain-text info)
  plain-text)

(defun org-typst-section (section contents info)
  contents)

(defun org-typst-paragraph (paragraph contents info)
  contents)

(defun org-typst-bold (bold contents info)
  (format "#strong[%s];" contents))

(defun org-typst-code (code contents info)
  (format "#raw[%s];" contents))

(defun org-typst-entity (entity contents info)
  (org-element-property :utf-8 entity))

(defun org-typst-export-snippet (export-snippet contents info)
  (when (equal "typst" (org-element-property :back-end export-snippet))
    (org-element-property :value export-snippet)))

(defun org-typst-footnote-reference (footnote-reference contents info)
  (format " #footnote[%s];"
          (car (org-export-get-footnote-definition footnote-reference info))))

(defun org-typst-inline-src-block (inline-src-block contents info)
  (format "#raw(\"%s\", block: false, lang: \"%s\");"
          (org-typst--escape-double-quote-in-string
           (org-element-property :value inline-src-block))
          (let ((org-lang (org-element-property :language inline-src-block)))
            (or (alist-get (intern org-lang) (plist-get info :typst-langs))
                (downcase org-lang)))))

(defun org-typst-italic (italic contents info)
  (format "#emph[%s];" contents))

(defun org-typst-line-break (line-break contents info)
  (format "" contents))

(defun org-typst-macro (macro contents info)
  (format "fml"))

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
    (macro . org-typst-macro)
    (radio-target . org-typst-radio-target)
    (link . org-typst-link))
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
