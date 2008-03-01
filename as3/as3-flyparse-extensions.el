(require 'flyparse-mode)
(require 'as3-mode)

(add-hook 'as3-mode-hook
          (lambda ()
	    (setq flyparse-parse-cmd as3-flyparse-parse-cmd)
	    (flyparse-mode-on)
	    ))

(defvar as3-flyparse-parse-cmd 
  '("java" "emacs.flyparse.as3.AS3Driver")
  "The shell command used to invoke the actionscript 3 parser.")

(defvar as3-flyparse-path-to-import-def
  '("COMPILATION_UNIT" "PACKAGE_DECL" "IMPORT_DEF"))

(defvar as3-flyparse-path-to-class-def
  '("COMPILATION_UNIT" "PACKAGE_DECL" "CLASS_DEF"))

(defvar as3-flyparse-path-to-class-ident
  (append as3-flyparse-path-to-class-def '("CLASS_NAME" "NAME")))

(defvar as3-flyparse-path-to-class-name
  (append as3-flyparse-path-to-class-ident '(*)))

(defvar as3-flyparse-path-to-extends-clause
  (append as3-flyparse-path-to-class-def '("EXTENDS_CLAUSE")))

(defvar as3-flyparse-path-to-extends-name
  (append as3-flyparse-path-to-extends-clause '("NAME" *)))

(defvar as3-flyparse-path-to-class-block
  (append as3-flyparse-path-to-class-def '("TYPE_BLOCK")))

(defvar as3-flyparse-path-to-class-member
  (append as3-flyparse-path-to-class-block '("CLASS_MEMBER")))

(defvar as3-flyparse-path-to-method-def
  (append as3-flyparse-path-to-class-member '("METHOD_DEF")))

(defvar as3-flyparse-path-to-method-name
  (append as3-flyparse-path-to-method-def '("METHOD_NAME" "NAME")))

(defvar as3-flyparse-path-to-method-param
  (append as3-flyparse-path-to-method-def '("PARAMS" "PARAM")))

(defvar as3-flyparse-path-to-method-return-type
  (append as3-flyparse-path-to-method-def '("TYPE_SPEC" "TYPE")))

(defvar as3-flyparse-path-to-method-name-text
  (append as3-flyparse-path-to-method-def '("METHOD_NAME" "NAME" *)))

(defvar as3-flyparse-path-to-method-def-block
  (append as3-flyparse-path-to-method-def '("BLOCK")))

(defvar as3-flyparse-path-to-variable-def
  (append as3-flyparse-path-to-class-member '("VARIABLE_DEF")))

(defvar as3-flyparse-path-to-variable-def-name
  (append as3-flyparse-path-to-variable-def '("VAR_DECLARATION" "NAME")))


;;;;;;;;;;;;;;;;;;;;;;
;; Regression Tests ;;
;;;;;;;;;;;;;;;;;;;;;;


(defun as3-flyparse-run-tests ()
  "Regression tests for the flyparse actionscript 3 grammar."
  (interactive)
  (let ((cmd as3-flyparse-parse-cmd))
    
    ;; Simple queries on imports
    (let* ((tree (flyparse-tree-for-string cmd "package aemon{ import com.aemon; import dog; import horse.*; public class Dude{}}")))
      (assert (= 3 
		 (length (flyparse-query-all as3-flyparse-path-to-import-def tree)))))

    ;; Simple queries on a class
    (let* ((tree (flyparse-tree-for-string cmd "package aemon{class Dude{}}")))
      (assert (= 1 
		 (length (flyparse-query-all as3-flyparse-path-to-class-def tree))))
      (assert (equal
	       "Dude"
	       (flyparse-tree-type (flyparse-query-first as3-flyparse-path-to-class-name tree)))))
    
    ;; Simple queries on a class's extends clause
    (let* ((tree (flyparse-tree-for-string cmd "package aemon{class Dude extends Man{}}")))
      (assert (equal "EXTENDS_CLAUSE" 
		     (flyparse-tree-type (flyparse-query-first as3-flyparse-path-to-extends-clause tree))))
      (assert (equal "Man" 
		     (flyparse-tree-type (flyparse-query-first as3-flyparse-path-to-extends-name tree)))))
    
    ;; Query for different types of for loop
    (let* ((tree (flyparse-tree-for-string cmd (concat "package aemon{class Dude{"
						       "public function Dude(){"
						       "   for(var name in hash){trace(name)}"
						       "   for(var i:Number = 0; i < 20; i++){trace(i);}"
						       "   for each(var ea:Thing in myThings){trace(ea);}"
						       "}"
						       "}}"))))
      (assert (= 1 (length
		    (flyparse-query-all (append as3-flyparse-path-to-method-def-block '("FOR_LOOP")) tree))))
      (assert (= 1 (length
		    (flyparse-query-all (append as3-flyparse-path-to-method-def-block '("FOR_IN_LOOP")) tree))))
      (assert (= 1 (length
		    (flyparse-query-all (append as3-flyparse-path-to-method-def-block '("FOR_EACH_LOOP")) tree)))))
    
    ;; Simple queries on friend class
    (let* ((tree (flyparse-tree-for-string cmd "package aemon{class Dude{}} class Dudette{private var monkey:Number;}")))
      (assert (equal "CLASS_DEF"
		     (flyparse-tree-type (flyparse-directed-search '("CLASS_DEF") 45 tree))))
      (assert (equal "VARIABLE_DEF"
		     (flyparse-tree-type (flyparse-directed-search '("VARIABLE_DEF") 45 tree)))))
    
    
    ;; Query for constant variable def in a friend class and it's value
    (let* ((tree (flyparse-tree-for-string cmd "package aemon{class Dude{}} class Dudette{public static const monkey:Number = 20;}")))
      (assert (equal "VARIABLE_DEF"
		     (flyparse-tree-type (flyparse-directed-search '("VARIABLE_DEF" (has ("VARIABLE_DEF" "const"))) 45 tree))))
      (assert (equal "VAR_INITIALIZER"
		     (flyparse-tree-type (flyparse-query-first '(("VARIABLE_DEF" (has ("VARIABLE_DEF" "const"))) 
								 "VAR_DECLARATION" "VAR_INITIALIZER") 
							       (flyparse-directed-search 
								'("VARIABLE_DEF" (has ("VARIABLE_DEF" "const"))) 45 tree))))))
    
    ;; test non-qualified function positioning...
    (let* ((tree (flyparse-tree-for-string cmd "package aemon{class Dude{public function Dude(){horse();}}}")))
      (assert (equal "horse"
		     (flyparse-tree-type (flyparse-directed-search '("horse") 52 tree)))))
    
    
    ;; query for super
    (let* ((tree (flyparse-tree-for-string cmd "package aemon{class Dude{public function Dude(){super();}}}")))
      (assert (equal "super" (flyparse-tree-type (flyparse-directed-search '("super") 52 tree)))))
    
    ;; search for constant variable reference
    (let* ((tree (flyparse-tree-for-string cmd "package aemon{class Dude{public function Dude(){return MOOSE;}}}")))
      (assert (equal "MOOSE" (flyparse-tree-as-text (flyparse-directed-search '("NAME") 58 tree)))))
    
    ;; literals passed to 'new' expression
    (let* ((tree (flyparse-tree-for-string cmd "package aemon{class Dude{public function Dude(){var aemon = new Crap({name: \"lkj\"});}}}")))
      (assert (equal "CONSTANT" (flyparse-tree-type (flyparse-directed-search '("CONSTANT") 78 tree)))))
    
    
    ;; new expression with Non-class expression
    (let* ((tree (flyparse-tree-for-string cmd "package aemon{class Dude{public function Dude(){var aemon = new crap({name: \"lkj\"});}}}")))
      (assert (equal "CONSTANT" (flyparse-tree-type (flyparse-directed-search '("CONSTANT") 78 tree)))))
    
    
    ;; Simple queries on a method definition
    (let* ((tree (flyparse-tree-for-string cmd "package aemon{class Dude{public function dude(dude:Dude, cat:Cat):Butt {touch()}}}")))
      (assert (= 1 
		 (length (flyparse-query-all as3-flyparse-path-to-method-def tree))))
      (assert (equal "NAME" 
		     (flyparse-tree-type (flyparse-query-first as3-flyparse-path-to-method-name tree))))
      (assert (equal "PARAM" 
		     (flyparse-tree-type (flyparse-query-first as3-flyparse-path-to-method-param tree))))
      (assert (equal "Butt" 
		     (flyparse-tree-as-text (flyparse-query-first as3-flyparse-path-to-method-return-type tree))))
      (assert (equal "dude" 
		     (flyparse-tree-type (flyparse-query-first as3-flyparse-path-to-method-name-text tree))))
      )

    ;; query on ..rest style method param
    (let* ((tree (flyparse-tree-for-string cmd "package aemon{class Dude{public function dude(...rest:Array){touch()}}}")))
      (assert (equal "PARAM" 
		     (flyparse-tree-type (flyparse-query-first as3-flyparse-path-to-method-param tree))))
      )
    
    ;; Simple queries on a method call
    (let* ((tree (flyparse-tree-for-string cmd "package aemon{class Dude{public function dude(){touch(1,2)}}}")))
      (assert (= 1 
		 (length (flyparse-query-all (append as3-flyparse-path-to-method-def-block '("EXPR_STMNT" "EXPR_LIST" "FUNCTION_CALL")) tree))))
      (assert (= 2
		 (length (flyparse-query-all (append as3-flyparse-path-to-method-def-block '("EXPR_STMNT" "EXPR_LIST" "FUNCTION_CALL" "ARGUMENTS" "EXPR_LIST" *)) tree))))
      )

    ;; Use helpers to get properties of method
    (let* ((tree (flyparse-tree-for-string cmd "package aemon{class Dude{public function runHorse(dude:Dude, cat:Cat):Butt{touch()}}}"))
	   (meth-tree (flyparse-query-first as3-flyparse-path-to-method-def tree)))
      (assert (not (null meth-tree)))
      (assert (equal "runHorse" (as3-method-name meth-tree)))
      (assert (equal "Butt" (as3-method-return-type meth-tree)))
      (assert (equal '("public") (as3-method-modifiers meth-tree)))
      (assert (equal '("Dude" "Cat") (as3-method-parameter-types meth-tree)))
      )

    ;; User method-return-type helper on void method
    (let* ((tree (flyparse-tree-for-string cmd "package aemon{class Dude{public function runHorse(dude:Dude, cat:Cat):void{touch()}}}"))
	   (meth-tree (flyparse-query-first as3-flyparse-path-to-method-def tree)))
      (assert (not (null meth-tree)))
      (assert (equal "void" (as3-method-return-type meth-tree)))
      )

    ;; Inline function definitions 
    (let* ((tree (flyparse-tree-for-string cmd "package aemon{class Dude{public function runHorse(dude){var dude = function(){}; var dude = function(a){return true;}; helloDude()}}}")))
      (assert (equal "FUNC_DEF" 
		     (flyparse-tree-type 
		      (flyparse-directed-search '("FUNC_DEF") 78 tree))))
      (assert (equal "FUNC_DEF" 
		     (flyparse-tree-type 
		      (flyparse-directed-search '("FUNC_DEF") 109 tree)))))


    ;; Inline function definition with missing semicolon. This code fails to parse because of antlr's automatic error correction. After parsing
    ;; 'true', antlr looks for a semi and can't find one - it then tries to correct the situation by deleting the current tokem,  '}', and using
    ;; then following semi. 
    ;; We then end up being short a '}'.
    (let* ((tree (flyparse-tree-for-string cmd "package aemon{class Dude{public function runHorse(dude){var dude = function(a){return true}; }}}")))
      ;; WILL FAIL
      (assert (not (equal "FUNC_DEF" 
			  (flyparse-tree-type 
			   (flyparse-directed-search '("FUNC_DEF") 82 tree))))))

    (message "All tests passed :)")
    ))



;;;;;;;;;;;;;;;;;;;
;; AS3 utilities ;;
;;;;;;;;;;;;;;;;;;;

(defun as3-first-member-var-def (tree name)
  "Return the first member variable definition for a variable named 'name', 
   otherwise, if none is found, return nil."
  (flyparse-query-first
   (append 
    as3-flyparse-path-to-class-member
    `(("VARIABLE_DEF" (has ("VARIABLE_DEF" "VAR_DECLARATION" "NAME" ,name))))) tree))

(defun as3-first-method-def (tree name)
  "Return the first method definition for a method names 'name', 
   otherwise, if none is found, return nil."
  (flyparse-query-first
   (append 
    as3-flyparse-path-to-class-member 
    `(("METHOD_DEF" (has ("METHOD_DEF" "METHOD_NAME" "NAME" ,name))))) tree))

(defun as3-first-class-name (tree)
  "Return the first class name found in tree."
  (flyparse-tree-type 
   (flyparse-query-first
    as3-flyparse-path-to-class-name tree)))

(defun as3-name-at-point (pos)
  (let ((var-name-tree (flyparse-containing-tree-of-type '("NAME"))))
    (if var-name-tree
	(flyparse-tree-type (flyparse-query-first '("NAME" *) var-name-tree)))))

(defun as3-class-name ()
  "Return the name of the current class."
  (let ((class-ident (flyparse-query-first as3-flyparse-path-to-class-ident)))
    (if class-ident
	(flyparse-tree-as-text class-ident)
      nil)))

(defun as3-constant-type (constant-tree)
  "Examine a flyparse tree of 'constant' type, and return its actionscript type.'"
  (cond ((flyparse-has-subtree-of-type-p constant-tree "LITERAL_NUMBER") "Number")
	((flyparse-has-subtree-of-type-p constant-tree "LITERAL_STRING") "String")
	((flyparse-has-subtree-of-type-p constant-tree "LITERAL_REGEX") "RegExp")
	((flyparse-has-subtree-of-type-p constant-tree "LITERAL_XML") "XML")
	(t "Object")))


(defun as3-getter-for (var-def)
  "Return text of a variable getter for var-def tree."
  (let ((type (flyparse-query-first 
	       '("VARIABLE_DEF" "VAR_DECLARATION" "TYPE_SPEC" "TYPE") var-def))
	(name (flyparse-query-first 
	       '("VARIABLE_DEF" "VAR_DECLARATION" *) var-def)))
    (format "public function get %s():%s { return %s }"
	    (replace-regexp-in-string "_" "" (flyparse-tree-as-text name))
	    (flyparse-tree-as-text type)
	    (flyparse-tree-as-text name))))


(defun as3-setter-for (var-def)
  "Return text of a variable setter for var-def tree."
  (let ((type (flyparse-query-first 
	       '("VARIABLE_DEF" "VAR_DECLARATION" "TYPE_SPEC" "TYPE") var-def))
	(name (flyparse-query-first 
	       '("VARIABLE_DEF" "VAR_DECLARATION" *) var-def)))
    (format "public function set %s(val:%s):void { %s = val }"
	    (replace-regexp-in-string "_" "" (flyparse-tree-as-text name))
	    (flyparse-tree-as-text type)
	    (flyparse-tree-as-text name))))

(defun as3-method-name (tree)
  (flyparse-tree-as-text (flyparse-query-first '("METHOD_DEF" "METHOD_NAME" "NAME") tree)))

(defun as3-method-return-type (tree)
  (let ((type-tree (flyparse-query-first '("METHOD_DEF" "TYPE_SPEC" "TYPE") tree)))
    (if (null type-tree)
	"void"
      (flyparse-tree-as-text type-tree))))

(defun as3-method-modifiers (tree)
  (mapcar (lambda (ea) (flyparse-tree-as-text ea))
	  (flyparse-query-all '("METHOD_DEF" "MODIFIER_LIST" *) tree)))

(defun as3-method-parameter-types (tree)
  (mapcar (lambda (ea) (flyparse-tree-as-text ea))
	  (flyparse-query-all '("METHOD_DEF" "PARAMS" "PARAM" "TYPE_SPEC" "TYPE") tree)))

(defun as3-pretty-method-desc (tree)
  "Return the a pretty stringified description of tree (a method tree)"
  (let* ((name (as3-method-name tree))
	 (type (as3-method-return-type tree))
	 (param-types (as3-method-parameter-types tree))
	 (modifiers (as3-method-modifiers tree))
	 )
    (format "%s %s%s:%s" (or modifiers "") name (or param-types "()") type)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AS3 Interactive Commands ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun as3-alphabetize-imports (pos)
  "Alphabetize the imports in this class."
  (interactive (list (point)))
  (let ((imports (flyparse-query-all as3-flyparse-path-to-import-def)))
    (if imports
	(let ((beg-point (flyparse-tree-beg-offset (first imports))))
	  (goto-char beg-point)
	  (flyparse-kill-region imports)
	  (let ((alphabetized-imports 
		 (sort imports (lambda (a b)
				 (string-lessp (flyparse-tree-as-text a)
					       (flyparse-tree-as-text b))))))
	    (mapc (lambda (ea)
		    (insert (flyparse-tree-as-text ea))
		    (end-of-line)
		    (newline))
		  alphabetized-imports)
	    (indent-region beg-point (point))
	    (kill-line)
	    ))
      (message "Could not find any imports."))))


(defun as3-switch-to-super ()
  "Open the file containing this this class's superclass and switch to it."
  (interactive)
  (let ((class-name-tree (flyparse-query-first 
			  as3-flyparse-path-to-class-name))
	(extends-name-tree (flyparse-query-first 
			    as3-flyparse-path-to-extends-name)))
    (if (and class-name-tree extends-name-tree)
	(let* ((class-name (flyparse-tree-type class-name-tree))
	       (extends-name (flyparse-tree-type extends-name-tree)))
	  (catch 'return-now
	    (flyparse-for-each-cached-tree 
	     (lambda (path tree)
	       (let ((class-name-tree (flyparse-query-first as3-flyparse-path-to-class-name tree)))
		 (if class-name-tree
		     (let ((class-name (flyparse-tree-type class-name-tree)))
		       (if (equal class-name extends-name)
			   (progn
			     (find-file-other-window path)
			     (throw 'return-now nil))))))))
	    (message "Sorry, could not locate superclass of %s." class-name)))
      (message "This buffer does not contain an AS3 class with an extends clause."))))

(defun as3-switch-to-subclass ()
  "Offer all subclasses of current class as option, switch to chosen subclass."
  (interactive)
  (let ((class-name-tree (flyparse-query-first 
			  as3-flyparse-path-to-class-name)))
    (if class-name-tree
	(let ((class-name (flyparse-tree-type class-name-tree))
	      (found-subclasses '()))
	  (flyparse-for-each-cached-tree 
	   (lambda (path tree)
	     (let ((subclass-name-tree (flyparse-query-first 
					as3-flyparse-path-to-class-name tree))
		   (extends-name-tree (flyparse-query-first 
				       as3-flyparse-path-to-extends-name tree)))
	       (if (and extends-name-tree subclass-name-tree)
		   (let ((subclass-name (flyparse-tree-type subclass-name-tree))
			 (extends-name (flyparse-tree-type extends-name-tree)))
		     (if (equal class-name extends-name)
			 (push `(,subclass-name . ,path) found-subclasses)))))))
	  (if (not (null found-subclasses))
	      (let* ((key (ido-completing-read "Select a subclass: "
					       found-subclasses 
					       nil t nil))
		     (path (cdr (assoc key found-subclasses))))
		(find-file-other-window path))
	    (message "Sorry, did not find any subclasses for %s." class-name)))
      (message "This buffer does not contain an AS3 class."))))


(defun as3-show-all-method-names ()
  "Just a test of flyparse's capabilities"
  (interactive)
  (let* ((method-name-trees (flyparse-query-all as3-flyparse-path-to-method-name-text))
	 (method-names (mapcar (lambda (tree) (flyparse-tree-type tree)) method-name-trees)))
    (message "%s" method-names)))


(defun as3-show-members-of (pos)
  "List the members of the class under point in a temp buffer."
  (interactive (list (point)))
  (let ((name (as3-name-at-point pos)))
    (if name
	(let ((found nil))
	  (flyparse-for-each-cached-tree
	   (lambda (path tree)
	     (let ((class-name-tree (flyparse-query-first 
				     as3-flyparse-path-to-class-name tree)))
	       (if class-name-tree
		   (let ((class-name (flyparse-tree-type class-name-tree)))
		     (if (equal class-name name)
			 (setf found `(,path ,tree))))))))
	  (if (not (null found))
	      (let ((path (first found))
		    (tree (second found)))
		(switch-to-buffer-other-window (format "*%s members*" name))
		(insert "\n\n")
		(let* ((method-trees (flyparse-query-all as3-flyparse-path-to-method-def tree)))
		  (mapc (lambda (meth-tree) 
			  (insert (format "%s\n\n" (as3-pretty-method-desc meth-tree))))
			method-trees))
		(goto-char (point-min)))
	    (message "Sorry, did not find class for %s." name)))
      (message "Not positioned over class name."))))


(defun as3-hoist-as-method (beg end)
  "Hoist a collection of statements into their own method."
  (interactive (list (mark) (point)))
  (let ((subtrees (flyparse-subtrees-contained-in-region beg end))
	(in-method (flyparse-containing-tree-of-type "METHOD_DEF")))
    
    (if (and in-method (not (null subtrees)))
	(let ((content-string (flyparse-region-to-string subtrees))
	      (method-name (read-string "Method name: ")))
	  (goto-char (+ 1 (flyparse-tree-end-offset in-method)))
	  (insert (format (concat
			   "\n\nprivate function %s(){\n"
			   "%s"
			   "\n}\n"
			   ) method-name content-string))
	  (goto-char (flyparse-tree-beg-offset (first subtrees)))
	  (flyparse-kill-region subtrees)
	  (insert (format "%s();" method-name))
	  (indent-region (point-min) (point-max)))
      (message "Must select at least one statement within a method."))))

(defun as3-new-import (import-name)
  "Add a new import statement."
  (interactive (list (read-string "Import identifier: ")))
  (let ((last-import (flyparse-query-last as3-flyparse-path-to-import-def)))
    (if last-import
	(progn
	  (goto-char (flyparse-tree-end-offset last-import))
	  (end-of-line)
	  (newline)
	  (insert (format "import %s;" import-name))
	  (indent-according-to-mode))
      (message "Could not find import list."))))

(defun as3-goto-first-boolean-method ()
  "Just a test of flyparse's capabilities"
  (interactive)
  (let ((method (flyparse-query-first 
		 (append as3-flyparse-path-to-class-member
			 '(("METHOD_DEF" (has ("METHOD_DEF" "TYPE_SPEC" "TYPE" "NAME" "Boolean"))))))))
    (if method
	(goto-char (flyparse-tree-beg-offset method)))))




(defun as3-kill-this-method (pos)
  "Remove a method to the kill ring."
  (interactive (list (point)))
  (let ((method (flyparse-query-first 
		 (append as3-flyparse-path-to-class-block '(("CLASS_MEMBER" in) "METHOD_DEF")))))
    (if method
	(flyparse-kill-tree method)
      (message "Not inside a method."))))


(defun as3-kill-all-but-constructor ()
  "Remove a method to the kill ring."
  (interactive)
  (let ((class-name (as3-class-name)))
    (if class-name
	(flyparse-kill-trees 
	 (flyparse-query 
	  `("COMPILATION_UNIT" "PACKAGE_DECL" "CLASS_DEF" "TYPE_BLOCK" 
	    ("CLASS_MEMBER" (has-none ("CLASS_MEMBER" "METHOD_DEF" ("METHOD_NAME" (text-match ,class-name))))))
	  flyparse-newest-parse-tree))
      (message "Couldn't get current class's name"))))


(defun as3-clear-this-method (pos)
  "Remove all statements from a method, leaving only the signature and open/close braces."
  (interactive (list (point)))
  (let ((code (flyparse-query-first 
	       (append as3-flyparse-path-to-class-block '(("CLASS_MEMBER" in) "METHOD_DEF" "BLOCK")))))
    (if code
	(progn
	  (goto-char (+ 1 (flyparse-tree-beg-offset code)))
	  (kill-region (+ 1 (flyparse-tree-beg-offset code))
		       (flyparse-tree-end-offset code))
	  (newline)
	  (newline)
	  (indent-according-to-mode)
	  (previous-line)
	  (indent-according-to-mode))
      (message "Not inside a method."))))

(defun as3-copy-contents-of-method (pos)
  "Save all statements from a method to the kill ring."
  (interactive (list (point)))
  (let ((code (flyparse-query-first 
	       (append as3-flyparse-path-to-class-block '(("CLASS_MEMBER" in) "METHOD_DEF" "BLOCK")))))
    (if code
	(progn
	  (goto-char (+ 1 (flyparse-tree-beg-offset code)))
	  (kill-ring-save (+ 1 (flyparse-tree-beg-offset code))
			  (flyparse-tree-end-offset code))
	  (message "Saved method contents to kill-ring."))
      (message "Not inside a method."))))


(defun as3-create-private-var (pos)
  (interactive (list (point)))
  (let* ((name (read-string "Please enter the variable name: "))
	 (type (read-string "Please enter the variable type: " "Number")))
    (insert (format "private var _%s:%s;" name type))))


(defun as3-create-getter (pos)
  "Create a getter for the var definition under point."
  (interactive (list (point)))
  (let ((var-def (flyparse-directed-search '("VARIABLE_DEF"))))
    (if var-def
	(progn
	  (end-of-line)
	  (newline)
	  (insert (as3-getter-for var-def))
	  (indent-according-to-mode))
      (message "Not inside a variable definition."))))

(defun as3-create-getter-and-setter (pos)
  "Create a getter and a setter for the var definition under point."
  (interactive (list (point)))
  (let ((var-def (flyparse-query-first 
		  (append as3-flyparse-path-to-class-block '(("CLASS_MEMBER" in) "VARIABLE_DEF")))))
    (if var-def
	(progn
	  (end-of-line)
	  (newline)
	  (insert (as3-getter-for var-def))
	  (indent-according-to-mode)
	  (end-of-line)
	  (newline)
	  (insert (as3-setter-for var-def))
	  (indent-according-to-mode))
      (message "Not inside a variable definition."))))

(defun as3-create-setter (pos)
  "Create a setter for the var definition under point."
  (interactive (list (point)))
  (let ((var-def (flyparse-query-first 
		  (append as3-flyparse-path-to-class-block '(("CLASS_MEMBER" in) "VARIABLE_DEF")))))
    (if var-def
	(progn
	  (end-of-line)
	  (newline)
	  (insert (as3-setter-for var-def))
	  (indent-according-to-mode))
      (message "Not inside a variable definition."))))

(defun as3-copy-method (pos)
  "Save all statements from a method to the kill ring."
  (interactive (list (point)))
  (let ((method (flyparse-query-first 
		 (append as3-flyparse-path-to-class-block '(("CLASS_MEMBER" in) "METHOD_DEF")))))
    (if method
	(progn
	  (goto-char (flyparse-tree-beg-offset method))
	  (kill-ring-save (flyparse-tree-beg-offset method)
			  (+ 1 (flyparse-tree-end-offset method)))
	  (message "Saved method to kill-ring."))
      (message "Not inside a method."))))

(defun as3-create-subclass ()
  "Create and switch to a new buffer containing a subclass of the current class."
  (interactive)
  (let ((class-name-tree (flyparse-query-first as3-flyparse-path-to-class-name))
	(extends-clause-tree (flyparse-query-first as3-flyparse-path-to-extends-clause)))
    (if class-name-tree
	(let* ((class-name (flyparse-tree-type class-name-tree))
	       (class-path (buffer-file-name))
	       (class-dir-path (file-name-directory class-path))
	       (newclass-name (replace-regexp-in-string 
			       "\\.as" "" 
			       (read-string "Please enter the subclass name: ")))
	       (newclass-file-name (concat newclass-name ".as"))
	       (newclass-path (concat class-dir-path newclass-file-name)))
	  (copy-file class-path newclass-path)
	  (find-file-other-window newclass-path)
	  (if extends-clause-tree
	      (flyparse-kill-tree extends-clause-tree))
	  (goto-char (+ 1 (flyparse-tree-end-offset class-name-tree)))
	  (insert (format " extends %s" class-name))
	  (goto-char 1)
	  (query-replace-regexp class-name newclass-name)
	  (save-buffer) ;; So kill-buffer doesn't ask 'are you sure?'
	  (let* ((response (read-string "Would you like to keep this class? (yes/no): "))
		 (approved-p (string-match "[yY][eE][sS]" response)))
	    (if (not approved-p)
		(progn
		  (delete-file newclass-path)
		  (kill-buffer-and-window)
		  )))
	  )
      (message "This is not a class.."))
    ))

(defun as3-create-from-template ()
  "Create and switch to a new buffer containing a class similiar to this one, 
   but named differently."
  (interactive)
  (let ((class-name-tree (flyparse-query-first as3-flyparse-path-to-class-name)))
    (if class-name-tree
	(let* ((class-name (flyparse-tree-type class-name-tree))
	       (class-path (buffer-file-name))
	       (class-dir-path (file-name-directory class-path))
	       (newclass-name (replace-regexp-in-string 
			       "\\.as" "" 
			       (read-string "Please enter the new class name: ")))
	       (newclass-file-name (concat newclass-name ".as"))
	       (newclass-path (concat class-dir-path newclass-file-name)))
	  (copy-file class-path newclass-path)
	  (find-file-other-window newclass-path)
	  (goto-char 1)
	  (query-replace-regexp class-name newclass-name)
	  (save-buffer) ;; So kill-buffer doesn't ask 'are you sure?'
	  (let* ((response (read-string "Would you like to keep this class? (yes/no): "))
		 (approved-p (string-match "[yY][eE][sS]" response)))
	    (if (not approved-p)
		(progn
		  (delete-file newclass-path)
		  (kill-buffer-and-window)
		  )))
	  )
      (message "This is not a class.."))
    ))


(defun as3-edit-var-def (pos)
  "Edit the initial value of the var referenced under cursor, within this class. "
  (interactive (list (point)))
  (save-excursion
    (let ((var-ref (flyparse-directed-search '("NAME"))))
      (if var-ref
	  (let* ((var-name (flyparse-tree-as-text var-ref))
		 (var-def
		  (flyparse-query-first 
		   (append 
		    as3-flyparse-path-to-class-member
		    `(("VARIABLE_DEF" (has ("VARIABLE_DEF" "VAR_DECLARATION" "NAME" ,var-name))))))))
	    (if var-def
		(let ((var-init (flyparse-search '("VAR_INITIALIZER") var-def)))
		  (if var-init
		      (progn
			(goto-char (flyparse-tree-beg-offset var-init))
			(let ((new-value (read-string "Variable value: ")))
			  (flyparse-kill-tree var-init)
			  (insert (format "= %s" new-value))))
		    (progn
		      (goto-char (+ 1 (flyparse-tree-end-offset var-def)))
		      (let ((new-value (read-string "Variable value: ")))
			(insert (format " = %s" new-value))))
		    ))
	      (message "Definition for %s not found." var-name)))
	(message "Not positioned in variable reference.")))))


(defun as3-goto-def (pos)
  "Jump to the definition  of the variable or method referenced under cursor, 
   might be in another file."
  (interactive (list (point)))
  (let ((obj-ref (flyparse-directed-search '("NAME"))))
    (if obj-ref
	(let* ((obj-name (flyparse-tree-as-text obj-ref))
	       (found-locations '()))
	  (flyparse-for-each-cached-tree 
	   (lambda (path tree)
	     (let ((found-def 
		    (or (as3-first-member-var-def tree obj-name)
			(as3-first-method-def tree obj-name))))
	       (if found-def 
		   (push `(,(file-name-sans-extension (file-name-nondirectory path)) . (,path ,found-def))
			 found-locations)))))
	  (if (not (null found-locations))
	      (let* ((key (if (= (length found-locations) 1)
			      (first (first found-locations))
			    (ido-completing-read 
			     "Found in these locations, pick one: "
			     found-locations 
			     nil t nil)
			    ))
		     (val (cdr (assoc key found-locations)))
		     (path (first val))
		     (def-tree (second val))
		     (offset (flyparse-tree-beg-offset def-tree)))
		(find-file-other-window path)
		(goto-char offset))
	    (message "Definition for %s not found." obj-name)))
      (message "Not positioned in variable or method name."))))

(defun as3-show-signature-for-word (pos)
  "Show the signature for the method name under cursor."
  (interactive (list (point)))
  (let ((meth-name (word-at-point))
	(potential-defs '()))
    (flyparse-for-each-cached-tree 
     (lambda (path tree)
       (let ((class-name (as3-first-class-name tree))
	     (found-def (as3-first-method-def tree meth-name)))
	 (if found-def 
	     (push (list class-name found-def) potential-defs)))))
    (if (not (null potential-defs))
	(message (mapconcat 
		  (lambda (ea) 
		    (format "%s#%s" 
			    (first ea) 
			    (as3-pretty-method-desc (second ea))))
		  potential-defs 
		  "\n"))
      (message "No definition for %s found." meth-name))))


(defun as3-hoist-as-constant (pos)
  "Create a new const static class-member with value equal to the constant
   under the cursor. Replace current literal value with reference to newly created
   constant. "
  (interactive (list (point)))
  (let ((constant (flyparse-containing-tree-of-type "CONSTANT" pos)))
    (if constant
	(let* ((const-type (as3-constant-type constant))
	       (last-const 
		(flyparse-query-last
		 (append as3-flyparse-path-to-variable-def '("const"))))
	       (last-var 
		(flyparse-query-last as3-flyparse-path-to-variable-def))
	       (type-block 
		(flyparse-query-first as3-flyparse-path-to-class-block))
	       (insertion-point 
		(cond (last-const (flyparse-tree-end-offset last-const))
		      (last-var (flyparse-tree-end-offset last-var))
		      (type-block (flyparse-tree-beg-offset type-block))
		      (t (throw 'FAILURE "Nowhere to insert constant :("))
		      )))
	  (let ((const-name (read-string "Name of constant: " ))
		(const-value (flyparse-tree-buffer-substring constant)))
	    (save-excursion
	      (goto-char (flyparse-tree-beg-offset constant))
	      (flyparse-kill-tree constant)
	      (insert const-name)
	      (goto-char insertion-point)
	      (end-of-line)
	      (newline)
	      (insert (format "public static const %s:%s = %s;" const-name const-type const-value))
	      (beginning-of-line)
	      (indent-according-to-mode))
	    (query-replace-regexp const-value const-name)))
      (message "Not positioned in constant."))))


(defun as3-copy-and-flip-line (pos)
  "Copy current line, insert it below, flipping width to height, x to y etc."
  (interactive (list (point)))
  (let* ((cur-line (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
	 (new-line (replace-regexp-in-string 
		    "horiz" "vert" 
		    (replace-regexp-in-string 
		     "width" "height"
		     (replace-regexp-in-string 
		      "x" "y" cur-line)))))
    (end-of-line)
    (newline)
    (beginning-of-line)
    (insert new-line)
    (indent-according-to-mode)
    ))

(defun as3-splice-exp-into-string (pos)
  (interactive (list (point)))
  (save-excursion
    (let ((string (flyparse-containing-tree-of-type "LITERAL_STRING" pos)))
      (if string 
	  (let ((exp-name (read-string "Expression to splice: " )))
	    (cond 
	     ((flyparse-has-subtree-of-type-p string "LITERAL_DOUBLE_STRING")
	      (insert (format "\" + %s  + \"" exp-name)))
	     
	     ((flyparse-has-subtree-of-type-p string "LITERAL_SINGLE_STRING")
	      (insert (format "' + %s  + '" exp-name)))))
	(message "Not positioned in string literal.")))))


(defun as3-paint-fill (pos)
  "Insert painting commands for the display object referenced at point."
  (interactive (list (point)))
  (let ((var-name (as3-name-at-point pos)))
    (if var-name
	(progn
	  (end-of-line)
	  (newline)
	  (insert (concat var-name ".graphics.lineStyle();\n"
			  var-name ".graphics.beginFill(0x000000, 0);\n"
			  var-name ".graphics.drawRect(0, 0, width, height);\n"
			  var-name ".graphics.endFill();"))
	  (indent-region pos (point))))))

(defun as3-contextual-double-quote (pos)
  (interactive (list (point)))
  (cond
   ((flyparse-containing-tree-of-type "LITERAL_DOUBLE_STRING" pos)
    (insert "\\\""))
   (t
    (insert "\""))))
;;(define-key as3-mode-map (kbd "\"") 'as3-contextual-double-quote)


(defun as3-contextual-single-quote (pos)
  (interactive (list (point)))
  (cond
   ((flyparse-containing-tree-of-type "LITERAL_SINGLE_STRING" pos)
    (insert "\\\'"))
   (t
    (insert "'"))))
;;(define-key as3-mode-map (kbd "'") 'as3-contextual-single-quote)

(defun as3-asdoc-method (pos)
  (interactive (list (point)))
  (let ((method-def (flyparse-containing-tree-of-type "METHOD_DEF")))
    (if method-def
	(let* ((params (flyparse-query-all '("METHOD_DEF" "PARAMS" "PARAM") method-def))
	       (return-type (flyparse-query-first '("METHOD_DEF" "TYPE_SPEC" "TYPE") method-def)))
	  (goto-char (flyparse-tree-beg-offset method-def))
	  (newline)
	  (previous-line)
	  (insert
	   (concat
	    "/**\n"
	    " * \n"
	    " * \n"
	    (mapconcat (lambda (param)
			 (concat "* @param " 
				 (flyparse-tree-as-text 
				  (flyparse-query-first '("PARAM" "NAME") param))
				 " \n"
				 )) params "")
	    " * @return \n"
	    "*/"
	    ))
	  (indent-region (flyparse-tree-beg-offset method-def)
			 (flyparse-tree-end-offset method-def)))
      (message "Not in a method."))))

(defun as3-asdoc-class (pos)
  (interactive (list (point)))
  (let ((class-def (flyparse-containing-tree-of-type "CLASS_DEF")))
    (if class-def
	(progn
	  (goto-char (flyparse-tree-beg-offset class-def))
	  (newline)
	  (previous-line)
	  (insert
	   (concat
	    "/**\n"
	    " * \n "
	    "*/"
	    ))
	  (indent-region (flyparse-tree-beg-offset class-def) (+ (point) 300)))
      (message "Not in a class."))))

(defvar as3-command-library 
  '(("hoist-constant" . "as3-hoist-as-constant")
    ("hoist-method" . "as3-hoist-as-method")
    ("clear-method" . "as3-clear-this-method")
    ("copy-method" . "as3-copy-method")
    ("copy-contents-of-method" . "as3-copy-contents-of-method")
    ("import" . "as3-new-import")
    ("kill-method" . "as3-kill-this-method")
    ("splice-exp-into-string" . "as3-splice-exp-into-string")
    ("accessors" . "as3-getter-setter")
    ("flip-to-y" . "as3-copy-and-flip-line")
    ("create-getter" . "as3-create-getter")
    ("create-setter" . "as3-create-setter")
    ("create-getter-and-setter" . "as3-create-getter-and-setter")
    ("create-subclass" . "as3-create-subclass")
    ("create-from-template" . "as3-create-from-template")
    ("create-private-var" . "as3-create-private-var")
    ("goto-def" . "as3-goto-def")
    ("edit-var-def" . "as3-edit-var-def")
    ("switch-to-super" . "as3-switch-to-super")
    ("switch-to-subclass" . "as3-switch-to-subclass")
    ("show-sig" . "as3-show-signature-for-word")
    ("paint-fill" . "as3-paint-fill")
    ("organize-imports" . "as3-alphabetize-imports")
    ("asdoc-method" . "as3-asdoc-method")
    ("asdoc-class" . "as3-asdoc-class")
    ("show-class-member-dir" . "as3-show-members-of")
    ("kill-all-members-except-constructor" . "as3-kill-all-but-constructor")
    ))

(defun as3-quick-menu ()
  (interactive)
  (run-command-by-bookmark as3-command-library))

(define-key as3-mode-map (kbd "C-c m") 'as3-quick-menu)

(define-key as3-mode-map (kbd "C-c d") 'as3-goto-def)

(provide 'as3-flyparse-extensions)