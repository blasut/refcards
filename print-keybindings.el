;;; -*- lexical-binding: t; -*-
(add-to-list 'load-path "~/code/emacs-which-key/")

(require 'which-key)

;; the unformatted bindings are like: (name .  function)
;; would be nice to also include the documentation

(defun get-doc (b)
  (let ((orig-desc (cdr b)))
    (when (and orig-desc
               (fboundp (intern orig-desc))
               (documentation (intern orig-desc)))
      (let* ((doc (documentation (intern orig-desc)))
             (str (replace-regexp-in-string "\n" " " doc))
             (max 100))
        (if (> (length str) max)
            (concat (substring str 0 max) "...")
          str)))))

(defun bindings-with-docs (bindings)
  (mapcar
   (lambda (b)
     `(key ,(car b) function ,(cdr b) doc ,(get-doc b)))
   bindings))

(defun pr-bindings--parse-key (key)
  (let* ((keys-to-be-cleaned '(("%" . "\\%")
                               ("&" . "\\&")
                               ("$" . "\\$")
                               ("#" . "\\#")
                               ("_" . "\\_")
                               ("{" . "\\{")
                               ("}" . "\\}")
                               ("[" . "{[}")
                               ("]" . "{]}")
                               ("+" . "\\texttt{+}")
                               ("~" . "\\textasciitilde")
                               ("^" . "\\textasciicircum")
                               ("\\" . "\\textbackslash")))
         (cleaned-key (replace-regexp-in-string (regexp-opt (mapcar 'car keys-to-be-cleaned))
                                                (lambda (x) (cdr (assoc-string x keys-to-be-cleaned)))
                                                key t t)))
    cleaned-key))

(pr-bindings--parse-key "M-^")
(pr-bindings--parse-key "%")

(make-string 1 ?\\)

(mapcar 'car '((a . b)))

(replace-regexp-in-string (regexp-opt '("%"))
                          (lambda (x) (cdr (assoc-string x '(("%" . "\\\\%")))))
                          "ak%bd" )

;; For each of the chars in the key string,
;; if it matches a special key, replace it with the nicer-representation

(replace-regexp-in-string "\#" "b" "a#b")

(pr-bindings--parse-key "^")
(pr-bindings--parse-key "a")

(defun binding->tex (binding)
  (let ((key (pr-bindings--parse-key (plist-get binding 'key)))
        (func-name (plist-get binding 'function))
        (doc (plist-get binding 'doc)))
    (concat "\t\\item"
            "["
            key
            "]"
            " "
            func-name)))

(binding->tex '(key "ESC" function "function-name" doc "documentation"))

(defun binding-with-widest-key (bindings)
  (car (seq-sort
        (lambda (a b)
          (> (length (plist-get a 'key))
             (length (plist-get b 'key))))
        bindings)))

(defun bindings->tex (bindings)
  (let ((widest-key (plist-get (binding-with-widest-key bindings) 'key)))
    (concat "\\begin{keylist}[labelwidth=\\widthof{\\keyify{" widest-key "}}]"
            "\n"
            (mapconcat 'identity
                       (cl-loop for binding in bindings
                                collect (binding->tex binding))
                       "\n")
            "\n"
            "\\end{keylist}"
            "\n")))

(bindings->tex '((key "ESC" function "function-name" doc "documentation")))

(defun filter-bindings-by-function-name (name bindings)
  (seq-filter (lambda (b) (string-match-p (regexp-quote name) (plist-get b 'function)))
              bindings))

(defun group-bindings-by (bindings func)
  (seq-group-by func bindings))

(defun grouped-binding->tex (grouped-binding)
  (concat "\\section"
          "{"
          (car grouped-binding)
          "}"
          "\n"
          (bindings->tex (cdr grouped-binding))
          "\n"))

(defun grouped-bindings->tex (grouped-bindings)
  (mapconcat 'identity
             (cl-loop for binding in grouped-bindings
                      collect (grouped-binding->tex binding))
             "\n"))

(defun save-bindings-to-file (filename bindings)
  (with-temp-file filename
    (insert (concat "\begin{multicols*}{3}"
                    bindings
                    "\n\n"
                    "\end{multicols*}{3}"
                    "\n\n"
                    "\\newpage"))))

;; :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;; Userland

;; maybe get the local key in the current local map?
;; see which-key: which-key--format-and-replace

(defun lispy-bindings ()
  (let* ((unformatted-bindings (sort (which-key--get-current-bindings) which-key-sort-order))
         (bindings (bindings-with-docs unformatted-bindings))
         (lispy-bindings (filter-bindings-by-function-name "lispy" bindings)))
    lispy-bindings))

(defun lispy-grouped-bindings (group-func)
  (let* ((lispy-bindings (lispy-bindings))
         (grouped-bindings (group-bindings-by lispy-bindings group-func)))
    grouped-bindings))

;; Grouping

(defun group-by-globals-locals-ace (b)
  (let ((key (plist-get b 'key))
        (func-name (plist-get b 'function)))
    (cond ((string-match-p (regexp-quote "-ace") func-name)
           "ace")
          ((string-match-p (regexp-opt '("<backtab>" "<escape>" "<C-return>" "<M-left>" "<M-return>" "<M-right>")) key)
           "long chars")
          ((string-match-p (regexp-opt '("[" "]" "{" "}" "\"\"" ";" "DEL" "C-" "M-")) key)
           "globals")
          (t
           "locals"))))

(defun group-by-globals-locals (b)
  (let ((match-name (plist-get b 'key)))
    (cond ((string-match-p (regexp-opt '("[" "]" "{" "}" "\"\"" ";" "DEL" "C-" "M-")) match-name)
           "globals")
          (t
           "locals"))))

(lispy-grouped-bindings 'group-by-globals-locals-ace)

(string-match-p (regexp-opt '("DEL" "C-" "M-")) "C-,")

(defun group-by-asl (b)
  (let ((func-name (plist-get b 'function)))
    (cond ((string-match-p (regexp-quote "-ace") func-name)
           "ace")
          ((string-match-p (regexp-quote "special") func-name)
           "special")
          ((string-match-p (regexp-quote "lispy") func-name)
           "lispy")
          (t
           func-name))))

(lispy-grouped-bindings 'group-by-asl)

(save-bindings-to-file "testing.tex" (grouped-bindings->tex (lispy-grouped-bindings 'group-by-globals-locals-ace)))

(save-bindings-to-file "testing.tex" (grouped-bindings->tex (lispy-grouped-bindings 'group-by-asl)))



;; This is sort of working, now we need to split up the result in columns, maybe filtering and/or adding section titles

;; First thing is trying to compile the code with TEX.
;; When it is working, then add filtering, I only want lispy/lispyville stuff
;; Then try creating some pdfsave-bindings-to-file (bindings->tex lispy-bindings) "testing.tex")


;; This is sorumns, and/or change the layouting of the tex. Landscape?

;; Might be able to use some other Tex formatting from some other refs?



