(require 'flyparse-mode)

(defvar css-flyparse-parse-cmd 
  '("java" "emacs.flyparse.css.CSSDriver"))


(add-hook 'css-mode-hook
          (lambda ()
	    (setq flyparse-parse-cmd css-flyparse-parse-cmd)
	    (flyparse-mode)
	    ))

(defun cssm-flyparse-declaration-name (tree)
  "Return the name of the css declaration described by 'tree'."
  (first (third tree)))

(defun cssm-flyparse-clear-ruleset (tree)
  "Kill all the declarations contained within rulset described by 'tree'."
  (let ((lbrace (flyparse-query-first '("RULE_SET" "{") tree))
	(rbrace (flyparse-query-first '("RULE_SET" "}") tree)))
    (kill-region (+ 1 (flyparse-tree-end-offset lbrace))
		 (- (flyparse-tree-beg-offset rbrace) 1))))


(defun cssm-alphabetize-declarations (pos)
  "Alphabetize the declarations in the current ruleset."
  (interactive (list (point)))
  (save-excursion
    (let* ((ruleset (flyparse-query-first '("STYLESHEET" ("RULE_SET" in)))))
      (cssm-alphabetize-declarations-in-ruleset ruleset)
      )))

(defun cssm-alphabetize-all-declarations ()
  "Alphabetize declarations in all rulesets."
  (interactive)
  (save-excursion
    (let ((rulesets (flyparse-query-all '("STYLESHEET" "RULE_SET"))))
      (dolist (ruleset (reverse rulesets))
	(cssm-alphabetize-declarations-in-ruleset ruleset))
      )))

(defun cssm-alphabetize-declarations-in-ruleset (ruleset)
  "Alphabetize the declarations in tree 'ruleset'."
  (let* ((decls (flyparse-query-all '("RULE_SET" "DECLARATION") ruleset))
	 (lbrace (flyparse-query-first '("RULE_SET" "{") ruleset)))
    (if decls
	(progn
	  (cssm-flyparse-clear-ruleset ruleset)
	  (goto-char (+ 1 (flyparse-tree-beg-offset lbrace)))
	  (newline)
	  (let ((alphabetized-decls 
		 (sort decls (lambda (a b)
			       (string-lessp (cssm-flyparse-declaration-name a)
					     (cssm-flyparse-declaration-name b))))))
	    (mapc (lambda (ea)
		    (insert (flyparse-tree-as-text ea))
		    (end-of-line)
		    (insert ";")
		    (newline)
		    ) alphabetized-decls)
	    (kill-line) ;; kill the extra newline
	    )))))


(provide 'css-flyparse-extensions)