;; Copyright (c) 2007 Aemon Cannon, aemoncannon -at- gmail -dot- com

;; Inspired by Pavel Kobyakov's flymake-mode

;; This file is part of flyparse-mode

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.



;; PLEASE NOTE:

;; Flyparse is not very useful as a stand-alone package. It is designed as 
;; a resource for language-modes. If you like to hack emacs lisp, and you 
;; want to add some powerful helper commands to your favorite language mode,
;; flyparse is for you.


;; INSTALLATION INSTRUCTIONS

;; * Put this file some place where emacs can find it.

;; * 'require' this file in your .emacs file e.g:
;;    (require 'flyparse-mode)

;; * The 'lib' subdirectory of the flyparse distribution a jar that 
;;   contains some parsers. Add this jar to your system's java classpath.

;; * Some things to try:
;;   Load up a supported filetype (css or js or as) - 

;;   Launch flyparse mode: M-x flyparse-mode

;;   Run the commands flyparse-pretty-print-tree or flyparse-toggle-debug-overlays
;;   These commands give different, visual representations of the parse-tree.

;;   Be advised that these visualizations will probably choke on big buffers 
;;   (even though flyparse at large will have no problems)


(eval-when-compile (require 'cl))

(defvar flyparse-log-buffer-name "*flyparse-log*"
  "Name of buffer to which log messages will be printed.")

(defvar flyparse-log-level 1
  "Logging level, only messages with level lower or equal will be logged.
  -1 = NONE, 0 = ERROR, 1 = WARNING, 2 = INFO, 3 = DEBUG")

(defvar flyparse-no-changes-timeout 0.4
  "Time to wait after last change before starting compilation.")

(defvar flyparse-debug-overlays-enabled nil
  "Illustrate parse-tree using overlays?")

(defvar flyparse-tree-cache (make-hash-table :test 'equal)
  "A hash table mapping file-system paths to parse trees.")

(defvar flyparse-debug-overlays '()
  "Time to wait after last change before starting compilation.")
(make-variable-buffer-local 'flyparse-debug-overlays)

(defvar flyparse-pending-patches '()
  "Track changes to buffer so that pending changes can be patched to trees as 
   they are loaded.")
(make-variable-buffer-local 'flyparse-pending-patches)

(defvar flyparse-timer nil
  "Timer for starting parse.")
(make-variable-buffer-local 'flyparse-timer)

(defvar flyparse-is-running nil
  "If t, flyparse parser process is running for the current buffer.")
(make-variable-buffer-local 'flyparse-is-running)

(defvar flyparse-parse-cmd nil
  "Shell command called to parse this buffer.")
(make-variable-buffer-local 'flyparse-parse-cmd)

(defvar flyparse-file-type-commands
  `(("\.as$" . ("java" "emacs.flyparse.as3.AS3Driver"))
    ("\.css$" . ("java" "emacs.flyparse.css.CSSDriver"))
    ("\.js$" . ("java" "emacs.flyparse.javascript.JavascriptDriver"))
    )
  "These are defaults. The value of flyparse-parse-cmd is preferred.")

(defvar flyparse-buffer-is-dirty nil
  "Has this buffer been modified since the last parse?")
(make-variable-buffer-local 'flyparse-buffer-is-dirty)

(defvar flyparse-error-in-last-parse nil
  "Non-nil if some condition is blocking a valid parse of the buffer.")
(make-variable-buffer-local 'flyparse-error-in-last-parse)

(defvar flyparse-last-change-time 0
  "Time of last buffer change.")
(make-variable-buffer-local 'flyparse-last-change-time)

(defvar flyparse-last-parse-time 0
  "Time of last successful parse.")
(make-variable-buffer-local 'flyparse-last-parse-time)

(defvar flyparse-parse-start-time 0
  "Time at which last parse was started.")
(make-variable-buffer-local 'flyparse-parse-start-time)

(defvar flyparse-newest-parse-tree nil
  "S-Exp tree from last parse.")
(make-variable-buffer-local 'flyparse-newest-parse-tree)

;; flyparse minor mode declarations
(defvar flyparse-mode-line nil)
(make-variable-buffer-local 'flyparse-mode-line)

(defvar flyparse-mode-line-e-w nil)
(make-variable-buffer-local 'flyparse-mode-line-e-w)

(defvar flyparse-mode-line-status nil)
(make-variable-buffer-local 'flyparse-mode-line-status)


;;;;;;;;;;;;;;;;;;;;;;
;; Public interface ;;
;;;;;;;;;;;;;;;;;;;;;;


(defun flyparse-has-subtree-of-type-p (tree type)
  "Does this 'tree' have a subtree of type 'type'?"
  (flyparse-depth-first-find tree
			     (lambda (ea buffer-offset depth)
			       (equal (flyparse-tree-type ea) type))))

(defun flyparse-syntax-stack (pos)
  "Message a stack of syntactic nodes representing the current syntax."
  (interactive (list (point)))
  (message "%s" (flyparse-syntax-stack-at-pos pos)))

(defun flyparse-search (query-segment &optional tree)
  "Search exaustively in 'tree' for the first subtree that matches 'query-segment'"
  (lexical-let ((tree (or tree flyparse-newest-parse-tree)))
    (flyparse-depth-first-find tree
			       (lambda (ea buffer-offset depth)
				 (flyparse-query-check ea 
						       query-segment 
						       buffer-offset)))))


(defun flyparse-directed-search (query-segment &optional pos tree)
  "Search 'tree' for the first subtree that both contains 'pos' and matches 'query-segment'"
  (lexical-let ((tree (or tree flyparse-newest-parse-tree))
		(pos (or pos (point))))
    (catch 'return-now
      (flyparse-depth-first-one-way-walk
       tree (lambda (ea buffer-offset)
	      (let ((contains-pos (flyparse-tree-contains-p ea pos buffer-offset)))
		(if (and contains-pos (flyparse-query-check ea query-segment buffer-offset))
		    (throw 'return-now (flyparse-absolute-tree-copy ea buffer-offset)))
		contains-pos
		)))
      nil
      )))

(defun flyparse-directed-search-containing-region (query-segment &optional beg end tree)
  "Search 'tree' for the first subtree that both contains 'beg' & 'end' and matches 'query-segment'"
  (lexical-let ((tree (or tree flyparse-newest-parse-tree))
		(beg (or beg (mark)))
		(end (or end (point))))
    (catch 'return-now
      (flyparse-depth-first-one-way-walk
       tree (lambda (ea ea-bo)
	      (let ((contains-region (and (flyparse-tree-contains-p ea beg ea-bo)
					  (flyparse-tree-contains-p ea end ea-bo))))
		(if (and contains-region (flyparse-query-check ea query-segment ea-bo))
		    (throw 'return-now (flyparse-absolute-tree-copy ea ea-bo)))
		contains-region
		)))
      nil
      )))

(defun flyparse-directed-search-smallest-containing-region (&optional beg end tree)
  "Search 'tree' for the smallest subtree that contains 'beg' & 'end'"
  (lexical-let ((tree (or tree flyparse-newest-parse-tree))
		(beg (or beg (mark)))
		(end (or end (point))))
    (let ((result (flyparse-depth-first-one-way-walk
		   tree (lambda (ea ea-bo)
			  (let ((contains-region (and (flyparse-tree-contains-p ea beg ea-bo)
						      (flyparse-tree-contains-p ea end ea-bo))))
			    (if contains-region (list ea ea-bo)))
			  ))))
      (let ((result-tree (first result))
	    (result-bo (second result)))
	(if result-tree (flyparse-absolute-tree-copy result-tree result-bo))))))

(defun flyparse-subtrees-contained-in-region (&optional beg end tree)
  "Return a list of all subtrees of 'tree' that are wholly contained in the region
   defined by 'beg' and 'end'"
  (let* ((tree (or tree flyparse-newest-parse-tree))
	 (beg (or beg (mark)))
	 (end (or end (point)))
	 (containing-tree (flyparse-directed-search-smallest-containing-region beg end tree))
	 (result '()))
    (if containing-tree
	(flyparse-each-subtree
	 (containing-tree ea 0 offset)
	 (if (and (>= (+ offset (flyparse-tree-beg-offset ea)) beg)
		  (<= (+ offset (flyparse-tree-end-offset ea)) end))
	     (push (flyparse-absolute-tree-copy ea offset) result))))
    (nreverse result)))

(defun flyparse-containing-tree-of-type (type &optional pos tree)
  "Search tree for first subtree of type 'type' that contains 'pos' - return an absolute version of subtree.
   Return nil if such a subtree does not contain 'pos'."
  (flyparse-directed-search type pos tree))

(defun flyparse-tree-buffer-substring (tree)
  "Return the buffer-substring delineated by this tree's offsets.'"
  (buffer-substring-no-properties (flyparse-tree-beg-offset tree)
				  (+ (flyparse-tree-end-offset tree) 1)))

(defun flyparse-kill-tree (tree)
  "Kill a tree to the kill-ring. Assumes the tree's offsets are absolute.'"
  (kill-region (flyparse-tree-beg-offset tree)
	       (+ (flyparse-tree-end-offset tree) 1)))

(defun flyparse-kill-trees (trees)
  "Kill all trees to the kill-ring. Assumes the trees'' offsets are absolute.'"
  (let ((ascending-trees (flyparse-sort-trees-asc trees)))
    (mapc (lambda (ea)
	    (flyparse-kill-tree ea))
	  ascending-trees)))

(defun flyparse-kill-region (trees)
  "Kill region bounded by absolute trees, from lowest offset to greatest."
  (let* ((region (flyparse-smallest-region-containing trees)))
    (kill-region (first region) (second region))))

(defun flyparse-kill-region-to-string (trees)
  "Kill region bounded by absolute trees, from lowest offset to greatest. Return
   the string bounded by the region"
  (let* ((region (flyparse-smallest-region-containing trees))
	 (str (buffer-substring-no-properties (first region) (second region))))
    (kill-region (first region) (second region))
    str))

(defun flyparse-region-to-string (trees)
  "Return the string bounded by 'trees'."
  (let* ((region (flyparse-smallest-region-containing trees))
	 (str (buffer-substring-no-properties (first region) (second region))))
    str))

(defun flyparse-smallest-region-containing (trees)
  (let* ((ascending-trees (flyparse-sort-trees-asc trees))
	 (beg (flyparse-tree-beg-offset (first (last ascending-trees))))
	 (end (+ 1 (flyparse-tree-end-offset (first ascending-trees)))))
    (list beg end)))

(defun flyparse-sort-trees-asc (trees)
  "Sort trees, non-destructively, in order of descending buffer offset. This ordering
   is useful when we wish to destructively modify the buffer (as we iterate over the trees,
   we may modify the corresponding buffer location without invalidating trees that occur
   later in the iteration)."
  (sort (copy-tree trees) (lambda (a b) (> (flyparse-tree-beg-offset a)
					   (flyparse-tree-beg-offset b)))))


(defun flyparse-query-all (query &optional tree)
  "Execute a query of the form: '(qualifier(s) qualifier(s) qualifier(s)) and return all subtrees with
   ancestry that matches the given sequence of types. The roots of all result trees will have absolute
   offsets."
  (flyparse-query query (or tree flyparse-newest-parse-tree)))

(defun flyparse-query-first (query &optional tree)
  "Execute a query with and return the first (textually speaking) matching subtree."
  (let ((matches (flyparse-query-all query (or tree flyparse-newest-parse-tree))))
    (if (not (null matches))
	(first matches)
      nil)))

(defun flyparse-query-last (query &optional tree)
  "Execute a query with and return the first (textually speaking) matching subtree."
  (let ((matches (flyparse-query-all query (or tree flyparse-newest-parse-tree))))
    (if (not (null matches))
	(first (last matches))
      nil)))

(defun flyparse-toggle-debug-overlays ()
  "Toggle the visibility of tree depth indicators."
  (interactive)
  (if flyparse-debug-overlays-enabled
      (progn
	(setq flyparse-debug-overlays-enabled nil)
	(flyparse-clear-debug-overlays))
    (progn
      (setq flyparse-debug-overlays-enabled t)
      (flyparse-refresh-debug-overlays))))


(defun flyparse-pretty-print-tree (tree)
  "Create a new buffer with an indented representation of the current
   buffer's parse-tree, then switch to it."
  (interactive (list flyparse-newest-parse-tree))
  (switch-to-buffer "*flyparse-pretty-print*")
  (kill-region (point-min) (point-max))
  (lexical-let ((tree tree))
    (flyparse-depth-first-walk
     tree
     (lambda (ea offset depth)
       (if (not (flyparse-leaf-p ea))
	   (progn
	     (newline)
	     (insert (make-string (* 3 depth) (string-to-char " "))))
	 (insert " "))
       (insert (format "(%s %s" (flyparse-tree-type ea) (flyparse-tree-info-list ea)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flyparse infrastructure ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun flyparse-cache-tree (tree path)
  "Add 'tree' to our global quick-lookup table of trees, indexed
   by 'path'. Path should be the absolute file-system path 
   to a source file."
  (puthash path tree flyparse-tree-cache))

(defun flyparse-get-cached-tree (path)
  "Return the tree associated with file-system path 'path' in
   the gobal path -> tree cache."
  (gethash path flyparse-tree-cache '()))

(defun flyparse-for-each-cached-tree (func &optional file-pattern)
  "Call 'func' for each tree in 'flyparse-tree-cache' where the
   corresponding source file path matches file-pattern."
  (maphash (lambda (path tree)
	     (if (or (and file-pattern (string-match file-pattern path))
		     (null file-pattern))
		 (funcall func path tree))) flyparse-tree-cache))

(defun flyparse-cache-all (dir file-pattern cmd-list)
  "Recursively iterate through files in 'dir' and cache the parse-tree 
   for each file whose full name matches the 'file-pattern' regex."
  (let ((counter 0))
    (flyparse-walk-path
     dir
     (lambda (ea-dir ea-file)
       (if (string-match file-pattern (concat ea-dir ea-file))
	   (let* ((file-path (expand-file-name (concat ea-dir ea-file)))
		  (tree (condition-case err
			    (flyparse-block-and-load-tree-from-file file-path cmd-list)
			  (error (progn
				   (message "Failed to parse %s" file-path)
				   nil)
				 ))))
	     (if tree
		 (progn
		   (flyparse-cache-tree tree file-path)
		   (message "Cached tree for %s" file-path)
		   (incf counter)))))))
    (message "Finished caching %s files." counter)))


(defun flyparse-write-cached-trees (dir-path)
  "Write flyparse-tree-cache to dir-path in a condensed eval-able format.
   See flyparse-read-cached-trees for reloading.
  "
  (interactive (list (ido-read-directory-name "Directory to write cache: ")))
  (let* ((temp-file-name (concat (or dir-path (file-name-directory buffer-file-name))
				 ".flyparse-tree-cache.el")))
    (with-temp-buffer
      (flyparse-for-each-cached-tree 
       (lambda (path tree)
	 (insert (format "(puthash %S '%S flyparse-loading-tree-cache)\n" path tree))
	 ))
      (let ((buffer-file-coding-system 'unix))
	(write-region (point-min) (point-max) temp-file-name nil 566))
      )
    (message "Finished writing %s" temp-file-name)
    ))

(defun flyparse-read-cached-trees (dir-path)
  "Read a flyparse-tree-cache from flat file. This operation will not overwrite existing trees."
  (interactive (list (ido-read-directory-name "Directory to read cache from: ")))
  (let* ((temp-file-name (concat (or dir-path (file-name-directory buffer-file-name))
				 ".flyparse-tree-cache.el")))
    (if (not (file-exists-p temp-file-name))
	(message "Error loading cached trees: %s not found." temp-file-name)
      (let ((counter 0)
	    (flyparse-loading-tree-cache (make-hash-table :test 'equal)))
	(load temp-file-name nil t)
	(maphash (lambda (path tree)
		   (if (not (gethash path flyparse-tree-cache '()))
		       (progn
			 (puthash path tree flyparse-tree-cache)
			 (incf counter)
			 )))
		 flyparse-loading-tree-cache)
	(message "Loaded %s trees from %s" counter temp-file-name)
	))
    ))


(defmacro flyparse-with-temp-cached-trees (mappings &rest body)
  "Exectute expressions in body in context of a temporary flyparse-tree-cache.
   Useful for unit testing in controlled environments."
  (let ((prepared-mappings `(list ,@(mapcar (lambda (ea) `(list ,(first ea) ,(second ea))) mappings))))
    `(let ((flyparse-tree-cache (make-hash-table :test 'equal)))
       (mapc (lambda (pair)
	       (let ((path (first pair))
		     (tree (second pair)))
		 (flyparse-cache-tree tree path)))
	     ,prepared-mappings)
       ,@body
       )))
  

(defun flyparse-clear-debug-overlays ()
  "Clear all debug overlays from this buffer."
  (interactive)
  (mapc (lambda (ea) 
	  (if (overlayp ea)
	      (delete-overlay ea)))
	flyparse-debug-overlays)
  (setq flyparse-debug-overlays '()))


(defun flyparse-refresh-debug-overlays ()
  "Use the current parse tree to redraw all the debug overlays."
  (interactive)
  (flyparse-clear-debug-overlays)
  (flyparse-depth-first-walk 
   flyparse-newest-parse-tree
   (lambda (ea offset depth)
     (push (flyparse-make-debug-overlay
	    (+ offset (flyparse-tree-beg-offset ea))
	    (+ 1 offset (flyparse-tree-end-offset ea))
	    (format "%s: %s to %s" (flyparse-tree-type ea)
		    (+ offset (flyparse-tree-beg-offset ea))
		    (+ 1 offset (flyparse-tree-end-offset ea)))
	    depth)
	   flyparse-debug-overlays))))


(defun flyparse-make-debug-overlay (beg end tooltip-text depth)
  "Allocate a flyparse overlay in range BEG and END."
  (when (not (gud-region-has-gud-overlays beg end))
    (let* ((ov (make-overlay beg end nil nil nil))
	   (color (concat "gray" (number-to-string (- 60 (* depth 3))))))
      (overlay-put ov 'face           `(background-color . ,color))
      (overlay-put ov 'help-echo      tooltip-text)
      (overlay-put ov 'flyparse-debug-overlay  t)
      (overlay-put ov 'priority depth)
      ov)))

(defalias 'flyparse-float-time
  (if (fboundp 'float-time)
      'float-time
    (if (featurep 'xemacs)
	(lambda ()
	  (multiple-value-bind (s0 s1 s2) (current-time)
	    (+ (* (float (ash 1 16)) s0) (float s1) (* 0.0000001 s2)))))))


(defun flyparse-save-buffer-in-file (file-name)
  "Save buffer to temp file. Use unix file coding:
   (NO CARRIAGE RETURNS (they will screw up the 
   stream index information))"
  (save-restriction
    (widen)
    (make-directory (file-name-directory file-name) 1)
    (let ((buffer-file-coding-system 'unix))
      (write-region (point-min) (point-max) file-name nil 566)))
  (flyparse-log 3 "Saved buffer %s in file %s." (buffer-name) file-name))


(defun flyparse-temp-parser-output-name (source-file-name)
  "For the given filename, return the name of the file to which the external parser process should
   write the parser tree."
  (let* ((postfix "flyparse_tree")
	 (extension (file-name-extension source-file-name)))
    (concat  (file-name-sans-extension source-file-name) "_" postfix "." extension)))


(defun flyparse-temp-buffer-copy-file-name (source-file-name)
  "Return the name of the file to which the given filename should be written for the 
   external parser-process to consume."
  (let* ((postfix "flyparse")
	 (extension (file-name-extension source-file-name)))
    (concat  (file-name-sans-extension source-file-name) "_" postfix "." extension)))


(defun flyparse-create-temp-buffer-copy ()
  "Make a temporary copy of the current buffer, 
   save its name in buffer data and return the name."
  (let*  ((temp-buffer-copy-name  (flyparse-temp-buffer-copy-file-name buffer-file-name)))
    (flyparse-save-buffer-in-file temp-buffer-copy-name)
    (flyparse-log 3 "Create-temp-inplace: file=%s temp=%s." buffer-file-name temp-buffer-copy-name)))


(defun flyparse-safe-delete-file (file-name)
  (when (and file-name (file-exists-p file-name))
    (delete-file file-name)
    (flyparse-log 3 "Deleted file %s" file-name)))

(defun flyparse-cleanup-temp-files (source-file-name)
  "Cleanup temporary files."
  (flyparse-safe-delete-file (flyparse-temp-buffer-copy-file-name source-file-name))
  (flyparse-safe-delete-file (flyparse-temp-parser-output-name source-file-name)))


(defun flyparse-cmd-for-file-type (file-name &optional cmd-list)
  "Return a command list appropriate to the file-type."
  (let* ((cmd-list (or cmd-list flyparse-file-type-commands))
	 (regex (first (first cmd-list)))
	 (cmd (rest (first cmd-list))))
    (or
     (if (string-match regex file-name) 
	 cmd)
     (if (not (null (rest cmd-list))) 
	 (flyparse-cmd-for-file-type file-name (rest cmd-list))))))


(defun flyparse-start-parse ()
  "Start a flyparse parse on current buffer."
  (interactive)
  (let* ((parser-cmd (or flyparse-parse-cmd 
			 (flyparse-cmd-for-file-type buffer-file-name)))
	 (cmd (first parser-cmd))
	 (args (rest parser-cmd)))
    (flyparse-create-temp-buffer-copy)
    (condition-case err
	(let ((proc (flyparse-create-parse-process 
		     cmd 
		     (append args (list (flyparse-temp-buffer-copy-file-name buffer-file-name) 
					(flyparse-temp-parser-output-name buffer-file-name))))))
	  (flyparse-log 2 "Created process %d, command=%s, dir=%s" 
			(process-id proc) (process-command proc)
			default-directory)
	  (set-process-sentinel proc 'flyparse-process-sentinel)
	  (set-process-filter proc 'flyparse-process-filter)
	  (process-put proc 'parser-output "")
	  (setq flyparse-is-running t)
	  (flyparse-clear-pending-patches)
	  (setq flyparse-parse-start-time (flyparse-float-time))
	  (flyparse-report-status "Running" "*"))
      (error
       (let* ((err-str (format "Failed to launch parser process '%s' with args %s: %s"
			       cmd args (error-message-string err))))
	 (flyparse-log 0 err-str)
	 (flyparse-cleanup-temp-files buffer-file-name)
	 )))))

(defun flyparse-create-parse-process (cmd args)
  "Start parse process. Return the emacs process object."
  (apply 'start-process "*flyparse-proc*" (current-buffer) cmd args))

(defun flyparse-process-filter (process output)
  "STDOUT of parser is already redirected to a file,
   so we'll only be receiving STDERR here..."
  (let ((source-buffer (process-buffer process))
	(parser-output-so-far (process-get process 'parser-output)))
    (process-put process 'parser-output (concat parser-output-so-far output))))

(defun flyparse-process-sentinel (process event)
  "Sentinel for flyparse buffers. Called automatically 
   when external parser process finishes."
  (when (memq (process-status process) '(signal exit))
    (let* ((exit-status       (process-exit-status process))
	   (command           (process-command process))
	   (source-buffer     (process-buffer process))
	   (source-file-name  (buffer-file-name source-buffer))
	   (proc-id           (process-id process))
	   (stderr (process-get process 'parser-output)))
      (flyparse-log 3 "Flyparse process %d exited with code %d" proc-id exit-status)
      (delete-process process)
      (if (buffer-live-p source-buffer)
	  (with-current-buffer source-buffer
	    (condition-case err
		(progn
		  (setq flyparse-is-running nil)
		  (setq flyparse-last-parse-time (flyparse-float-time))
		  (if (eq exit-status 0)
		      (let ((tree '())) ;; 'tree' will be set by the 'load'ed list..
			;; Load the parse tree...
			(load (flyparse-temp-parser-output-name source-file-name) nil t)
			(if (flyparse-tree-acceptable-p tree)
			    (flyparse-update-newest-parse-tree tree)
			  (error "Invalid parse tree.")))
		    (error "Non-Zero exit status."))
		  ;; Log stderr
		  (if (> (length stderr) 0) (flyparse-log 1 stderr))
		  (setq flyparse-error-in-last-parse nil)
		  (flyparse-cleanup-temp-files source-file-name)
		  )
	      (error
	       (let ((err-str (format "Flyparse Failed: %s: %s: %s" source-buffer (error-message-string err) stderr)))
		 (flyparse-report-status "Failed" "")
		 (flyparse-log 0 err-str)
		 (flyparse-cleanup-temp-files source-file-name)
		 (setq flyparse-error-in-last-parse t)
		 ))))

	;; Else, if source-buffer no longer exists
	(progn 
	  (flyparse-cleanup-temp-files source-file-name)
	  (flyparse-log 3 "Flyparse sentinel called back to dead buffer %d with code %d" proc-id exit-status))
	))))

(defun flyparse-tree-acceptable-p (tree)
  "A sentinel helper: Should this tree be installed as the 
   new value of 'flyparse-newest-parse-tree'?
   Check that 'tree' is a valid parse tree for the active buffer."
  (and (not (null tree))
       (< 0 (flyparse-tree-height tree))
       ))

(defun flyparse-update-newest-parse-tree (tree)
  "A sentinel helper: Install an updated parse tree for
   the active buffer. 
   If modifications took place during the parse,
   apply the patches."
  (setq flyparse-newest-parse-tree tree)
  (flyparse-cache-tree tree (buffer-file-name))
  ;; If buffer was modified during the parse:
  (if flyparse-pending-patches
      (progn
	(flyparse-apply-pending-patches)
	(flyparse-clear-pending-patches)
	(setq flyparse-buffer-is-dirty t)
	(flyparse-log 2 "Parse tree updated - still dirty.")
	(flyparse-report-status nil "*"))
    ;; Otherwise, all clean:
    (progn
      (setq flyparse-buffer-is-dirty nil)
      (flyparse-log 2 "Parse tree updated.")
      (flyparse-report-status "" "")))
  (when flyparse-debug-overlays-enabled
    (flyparse-refresh-debug-overlays)))

(defun flyparse-log-buffer ()
  "Return the current log buffer, creating it 
   if it doesn't exist."
  (get-buffer-create flyparse-log-buffer-name))

(defun flyparse-log (level text &rest args)
  "Log a message at level LEVEL.
   If LEVEL is higher than `flyparse-log-level', the message is
   only printed to the log buffer.  Otherwise, it is also printed using `message'.
   TEXT is a format control string, and the remaining arguments ARGS
   are the string substitutions (see `format')."
  (if (<= level flyparse-log-level)
      (with-current-buffer (flyparse-log-buffer)
	(let* ((msg (apply 'format text args)))
	  (goto-char (point-max))
	  (newline)
	  (insert msg)
	  ))))

(defun flyparse-report-status (e-w &optional status)
  "Show status in mode line."
  (when e-w
    (setq flyparse-mode-line-e-w e-w))
  (when status
    (setq flyparse-mode-line-status status))
  (let* ((mode-line " Flyparse"))
    (when (> (length flyparse-mode-line-e-w) 0)
      (setq mode-line (concat mode-line ":" flyparse-mode-line-e-w)))
    (setq mode-line (concat mode-line flyparse-mode-line-status))
    (setq flyparse-mode-line mode-line)
    (force-mode-line-update)))


(defun flyparse-after-save-hook ()
  (when (and (local-variable-p 'flyparse-mode (current-buffer))
	     (not flyparse-is-running))
    (progn
      (flyparse-log 3 "Starting syntax check as buffer was saved.")
      (flyparse-start-parse)
      )))

(defun flyparse-kill-buffer-hook ()
  (when flyparse-timer
    (cancel-timer flyparse-timer)
    (setq flyparse-timer nil)
    (flyparse-cleanup-temp-files buffer-file-name)
    ))

(defun flyparse-after-change-function (start stop len)
  "Start parse for current buffer if it isn't already running."
  (let* ((patch (list start stop len)))
    (flyparse-report-status nil "*")
    (setq flyparse-last-change-time (flyparse-float-time))
    (setq flyparse-buffer-is-dirty t)
    (if flyparse-newest-parse-tree
	(flyparse-patch-tree flyparse-newest-parse-tree patch))
    (push patch flyparse-pending-patches)
    (when flyparse-debug-overlays-enabled
      (flyparse-refresh-debug-overlays))
    ))

(defun flyparse-clear-pending-patches ()
  "Forget all buffer changes up to now."
  (setq flyparse-pending-patches '()))

(defun flyparse-apply-pending-patches ()
  "For each buffer change that has occurred since the last
   call to 'flyparse-clear-pending-patches', apply the change patch
   to the current parse-tree."
  (mapc (lambda (patch) (flyparse-patch-tree flyparse-newest-parse-tree patch))
	(reverse flyparse-pending-patches)))

(defun flyparse-on-timer-event (buffer)
  "Start a parse for buffer BUFFER if necessary."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and 
	     (not flyparse-is-running) ;; There's not a parse in progress and..
	     flyparse-buffer-is-dirty ;; ..the buffer is not already fully parsed and..
	     
	     (or (not flyparse-error-in-last-parse) ;; either we did NOT encounter an error in the last parse..
		 ;; or we did, but the buffer has since been changed :)
		 (and flyparse-error-in-last-parse (> flyparse-last-change-time flyparse-last-parse-time)))
	     
	     ;; aaand the time since the last change is greater than our threshold.
	     (> (- (flyparse-float-time) flyparse-last-change-time) flyparse-no-changes-timeout))
	
	(flyparse-log 3 "Starting parse.")
	(flyparse-start-parse)
	))))


(define-minor-mode flyparse-mode
  "Minor mode to do on-the-fly syntax parsing.
  When called interactively, toggles the minor mode.
  With arg, turn Flyparse mode on if and only if arg is positive."
  :group 'flyparse :lighter flyparse-mode-line
  (cond
   
   ;; Turning the mode ON.
   (flyparse-mode
    (add-hook 'after-change-functions 'flyparse-after-change-function nil t)
    (add-hook 'after-save-hook 'flyparse-after-save-hook nil t)
    (add-hook 'kill-buffer-hook 'flyparse-kill-buffer-hook nil t)
    ;;+(add-hook 'find-file-hook 'flyparse-find-file-hook)
      
    (flyparse-report-status "" "")
    (setq flyparse-timer (run-at-time nil 1 'flyparse-on-timer-event (current-buffer)))
    (flyparse-start-parse))
   
   ;; Turning the mode OFF.
   (t
    (remove-hook 'after-change-functions 'flyparse-after-change-function t)
    (remove-hook 'after-save-hook 'flyparse-after-save-hook t)
    (remove-hook 'kill-buffer-hook 'flyparse-kill-buffer-hook t)
    ;;+(remove-hook 'find-file-hook (function flyparse-find-file-hook) t)
    
    (when flyparse-timer
      (cancel-timer flyparse-timer)
      (setq flyparse-timer nil))
    
    (setq flyparse-is-running nil))))


(defun flyparse-mode-on ()
  "Turn flyparse mode on."
  (flyparse-mode 1)
  (flyparse-log 1 "Flyparse mode turned ON for buffer %s." (buffer-name)))


(defun flyparse-mode-off ()
  "Turn flyparse mode off."
  (flyparse-mode 0)
  (flyparse-log 1 "Flyparse mode turned OFF for buffer %s." (buffer-name)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Private utilities for manipulating parse trees ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun flyparse-patch-tree (tree patch)
  "Buffer modifications that take place between parses will
   not be reflected in the structure of the current parse tree - 
   i.e. the parse-tree can and will become out of date :(

   This function will update the offsets in 'tree' and its subtrees 
   to reflect interim buffer changes described by 'patch' of form (start stop len).
  
   XXX - This function could be improved considerably - currently, it only
   helps for simple buffer modifications."
  
  (lexical-let* ((beg (first patch))
		 (end (second patch))
		 (len (third patch))
		 (old-region-length len)
		 (new-region-length (- end beg))
		 (containing-tree nil)
		 (containing-tree-buffer-offset 0)
		 (old-beg beg)
		 (old-end (+ beg len))
		 )
    ;; Perform search of 'tree', searching for subtree of 'tree' that completely 
    ;; contains the pre-change buffer region. 
    ;; Update the end-offsets of all trees on the path to the containing tree.
    (flyparse-depth-first-one-way-walk
     tree (lambda (subtree buffer-offset)
	    (if (and (flyparse-tree-contains-p subtree old-beg buffer-offset)
		     (flyparse-tree-contains-p subtree old-end buffer-offset))
		(progn
		  (flyparse-set-tree-end-offset 
		   subtree
		   (+ (flyparse-tree-end-offset subtree)
		      (- new-region-length old-region-length)))
		  (setf containing-tree subtree)
		  (setf containing-tree-buffer-offset buffer-offset)
		  t)
	      nil
	      )))
    
    ;; Where necessary, update the offsets of the direct subtrees of above-mentioned
    ;; containing tree.
    (let ((subtrees (if (null containing-tree) (list tree)
		      (flyparse-tree-subtrees containing-tree)))
	  (buffer-offset containing-tree-buffer-offset))
      (catch 'finished 
	(dolist (ea subtrees)
	  (let ((subtree-beg (+ (flyparse-tree-beg-offset ea) buffer-offset))
		(subtree-end (+ (flyparse-tree-end-offset ea) buffer-offset)))
	    (if (and (< old-beg subtree-beg)
		     (< old-end subtree-beg))
		(progn
		  (flyparse-set-tree-beg-offset ea
						(+ (flyparse-tree-beg-offset ea)
						   (- new-region-length old-region-length)))
		  (flyparse-set-tree-end-offset ea
						(+ (flyparse-tree-end-offset ea)
						   (- new-region-length old-region-length)))
		  (throw 'finished nil)
		  )))
	  (setf buffer-offset (+ buffer-offset (flyparse-tree-end-offset ea)))
	  ))
      )))

(defun flyparse-depth-first-find (tree pred-func &optional buffer-offset depth)
  "Perform a depth-first search of the tree. Return the first subtree for which 'iterator-func'
   returns non-nil."
  (lexical-let* ((tree (or tree flyparse-newest-parse-tree))
		 (buffer-offset (or buffer-offset 0))
		 (depth (or depth 0))
		 (next-offset (flyparse-tree-beg-offset tree)))
    (catch 'return-now
      (flyparse-depth-first-walk tree (lambda (ea ea-bo ea-depth)
					(if (funcall pred-func ea ea-bo ea-depth)
					    (throw 'return-now (flyparse-absolute-tree-copy ea ea-bo))))
				 buffer-offset depth
				 ))))


(defun flyparse-query (query tree &optional buffer-offset)
  "Execute 'query' on 'tree', return all matches."
  (let* ((query-segment (first query))
	 (buffer-offset (or buffer-offset 0)))
    (if (flyparse-query-check tree query-segment buffer-offset)
	(flyparse-query-process-rest (rest query) tree buffer-offset))))


(defun flyparse-query-process-rest (remaining-query tree buffer-offset)
  "Worker function for flyparse queries."
  (if (null remaining-query)
      (list (flyparse-absolute-tree-copy tree buffer-offset))
    (let* ((matched-subtrees '())
	   (query-segment (first remaining-query)))
      (flyparse-each-subtree
       (tree ea buffer-offset offset)
       (if (flyparse-query-check ea query-segment offset)
	   (setf matched-subtrees 
		 (append 
		  matched-subtrees
		  (flyparse-query-process-rest (rest remaining-query) ea offset)))))
      matched-subtrees)))


(defun flyparse-query-check (tree query-segment buffer-offset)
  "A query helper. Return t or nil depending on whether query-segment matches tree."
  (cond
   ((null query-segment) t)
   
   ((listp query-segment) 
    (and (flyparse-query-check-qualifier tree (first query-segment) buffer-offset)
	 (flyparse-query-check tree (rest query-segment) buffer-offset)))
   
   (t (flyparse-query-check-qualifier tree query-segment buffer-offset))))


(defun flyparse-query-check-qualifier (tree qualifier buffer-offset)
  "A query helper. Return t or nil depending on whether qualifier matches tree."
  (if (null qualifier) t
    (cond
     
     ;; Process universal qualifier, *
     ((and (symbolp qualifier) (equal qualifier '*))
      t)
     
     ;; Process type qualifier e.g. "TYPE"
     ((stringp qualifier)
      (equal (flyparse-tree-type tree) qualifier))
     
     ;; Process simple option qualifier e.g. in
     ((symbolp qualifier)
      (let ((option-name qualifier))
	(flyparse-query-process-simple-qualifier tree option-name buffer-offset)))
     
     ;; Process complex option qualifier e.g. (at 12)
     ((and (listp qualifier) (symbolp (first qualifier)))
      (let ((option-name (first qualifier))
	    (args (rest qualifier)))
	(flyparse-query-process-complex-qualifier tree option-name args buffer-offset)))
     )))

(defun flyparse-query-process-simple-qualifier (tree option-name buffer-offset)
  "A query helper. Return t if this query option applies to this tree."
  (case option-name
    ('in (flyparse-tree-contains-p tree (point) buffer-offset))
    ))

(defun flyparse-query-process-complex-qualifier (tree option-name args buffer-offset)
  "A query helper. Return t if this query option applies to this tree."
  (case option-name
    ('at 
     (let ((pos (first args)))
       (flyparse-tree-contains-p tree pos buffer-offset)))
    
    ('has
     (let ((sub-query (first args)))
       (> (length (flyparse-query sub-query tree buffer-offset)) 0)))

    ('has-none
     (let ((sub-query (first args)))
       (= (length (flyparse-query sub-query tree buffer-offset)) 0)))

    ('text-match
     (let ((regex (first args)))
       (not (null (string-match regex (flyparse-tree-as-text tree))))))

    ))

(defun flyparse-tree-subtrees (tree)
  "Return a list of this tree's children."
  (if (< (length tree) 3) '()
    (cdr (cdr tree))))

(defun flyparse-tree-contains-p (tree pos &optional buffer-offset)
  "Is pos contained by the syntax defined by tree?
   'offset' is an integer offset from the start-char of this tree"
  (let ((buffer-offset (if buffer-offset buffer-offset 0)))
    (and (>= (- pos buffer-offset) (flyparse-tree-beg-offset tree))
	 (<= (- pos buffer-offset) (flyparse-tree-end-offset tree)))))

(defun flyparse-tree-info-list (tree)
  "Accessor tree's list of metadata."
  (second tree))

(defun flyparse-tree-height (tree)
  (- (flyparse-tree-end-offset tree) (flyparse-tree-beg-offset tree)))

(defun flyparse-tree-beg-offset (tree)
  (first (flyparse-tree-info-list tree)))

(defun flyparse-tree-end-offset (tree)
  (second (flyparse-tree-info-list tree)))

(defun flyparse-set-tree-beg-offset (tree offset)
  (setf (first (flyparse-tree-info-list tree)) offset))

(defun flyparse-set-tree-end-offset (tree offset)
  (setf (second (flyparse-tree-info-list tree)) offset))

(defun flyparse-tree-type (tree)
  (first tree))

(defun flyparse-leaf-p (tree)
  (= (length (flyparse-tree-subtrees tree)) 0))

(defun flyparse-tree-as-text (tree)
  "Naively destructure a flyparse tree to text representation."
  (if (not (null tree))
      (if (flyparse-leaf-p tree)
	  (flyparse-tree-type tree)
	(let ((result ""))
	  (flyparse-each-subtree
	   (tree subtree nil offset)
	   (let ((space-width (max 0 (- (+ offset (flyparse-tree-beg-offset subtree)) offset 1))))
	     (setf result (concat result (make-string space-width (string-to-char " "))))
	     (setf result (concat result (flyparse-tree-as-text subtree)))))
	  result))
    (error '"Tried to convert null tree to text")))


(defun flyparse-absolute-tree-copy (tree base-buffer-offset)
  "Make a copy of 'tree' with offsets of root tree converted
   to absolute buffer offsets."
  (let ((new-tree (copy-tree tree)))
    (flyparse-set-tree-beg-offset
     new-tree 
     (+ base-buffer-offset (flyparse-tree-beg-offset new-tree)))
    (flyparse-set-tree-end-offset
     new-tree 
     (+ base-buffer-offset (flyparse-tree-end-offset new-tree)))
    new-tree))


(defun flyparse-syntax-stack-at-pos (pos)
  "Return a stack of syntactic nodes representing the current syntax."
  (lexical-let ((syntax-stack '()))
    (flyparse-depth-first-one-way-walk
     flyparse-newest-parse-tree (lambda (tree buffer-offset)
				  (if (flyparse-tree-contains-p tree pos buffer-offset)
				      (push (flyparse-tree-type tree) syntax-stack)
				    nil
				    )))
    syntax-stack))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Traversal/Iteration Helpers ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro flyparse-each-subtree (args &rest body)
  "Execute body for each subtree of 'tree', passing each
   subtree and the current buffer-offset as parameters"
  (let ((tree-init-form (first args))
	(ea-subtree-form (second args))
	(buffer-offset-init-form (third args))
	(ea-bo-form (fourth args))
	(tree-form (gensym))
	(buffer-offset-form (gensym)))
    `(let* ((,tree-form ,tree-init-form)
	    (,buffer-offset-form ,buffer-offset-init-form)
	    (,ea-bo-form (if ,buffer-offset-form 
			     (+ ,buffer-offset-form
				(flyparse-tree-beg-offset ,tree-form))
			   (flyparse-tree-beg-offset ,tree-form)))
	    (subtrees (flyparse-tree-subtrees ,tree-form)))
       (dolist (,ea-subtree-form subtrees)
	 ,@body
	 (setf ,ea-bo-form (+ ,ea-bo-form (flyparse-tree-end-offset ,ea-subtree-form)))))))


(defun flyparse-depth-first-one-way-walk (tree iterator-func &rest buffer-offset)
  "Perform a one-way, directed walk of 'tree'. For each subtree, if
   'iterator-func' evaluates to non-nil, continue walking in that
   subtree. Return the last non-nil return value of 'iterator-func'.
   
   NOTE: This function does NOT backtrack. At each level, the choice
   of subtree to search is final. 
   
   i.e Use this function when there is a single, unique target and
   we know exactly which decisions to make to get to that target.
   "
  (let* ((tree (or tree flyparse-newest-parse-tree))
	 (buffer-offset (or buffer-offset 0))
	 (result (funcall iterator-func tree buffer-offset)))
    (if result
	(catch 'result
	  (while t
	    (catch 'continue
	      (flyparse-each-subtree 
	       (tree ea-subtree buffer-offset ea-bo)
	       (let ((sub-result (funcall iterator-func ea-subtree ea-bo)))
		 (if sub-result
		     (progn
		       (setf result sub-result)
		       (setf tree ea-subtree)
		       (setf buffer-offset ea-bo)
		       (throw 'continue nil)))))
	      (throw 'result result))))
      result)))


(defun flyparse-depth-first-walk (tree iterator-func &optional buffer-offset depth)
  "Perform a depth-first walk of the tree. Evaluating 'iterator-func'
   for each subtree, passing the subtree, buffer-offset, and depth as arguments."
  (let* ((tree (or tree flyparse-newest-parse-tree))
	 (buffer-offset (or buffer-offset 0))
	 (depth (or depth 0))
	 (tree-stack (list tree))
	 (depth-stack (list depth))
	 (offset-stack (list buffer-offset)))
    (while (not (null tree-stack))
      (let* ((this-tree (pop tree-stack))
	     (this-depth (pop depth-stack))
	     (this-offset (pop offset-stack))
	     (subtrees (flyparse-tree-subtrees this-tree))
	     (tmp-tree-stack '())
	     (tmp-depth-stack '())
	     (tmp-offset-stack '()))
	(funcall iterator-func this-tree this-offset this-depth)
	(flyparse-each-subtree 
	 (this-tree ea this-offset offset)
	 (push ea tmp-tree-stack)
	 (push (+ 1 this-depth) tmp-depth-stack)
	 (push offset tmp-offset-stack))
	(setf tree-stack (append (nreverse tmp-tree-stack) tree-stack))
	(setf depth-stack (append (nreverse tmp-depth-stack) depth-stack))
	(setf offset-stack (append (nreverse tmp-offset-stack) offset-stack))
	))))


;;;;;;;;;;;;;;;;;;;;;;
;; Regression tests ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun flyparse-run-tests ()
  "Run all regression tests for flyparse-mode."
  (interactive)
  
  ;; Test tree accessors
  (let* ((tree
	  '("ROOT" (0 20)
	    ("LEAF" (0 4)))))
    (assert (equal (flyparse-tree-beg-offset tree) 0))
    (assert (equal (flyparse-tree-end-offset tree) 20))
    (assert (equal (flyparse-tree-type tree) "ROOT"))
    (assert (not (flyparse-leaf-p tree)))
    (assert (equal (flyparse-tree-subtrees tree) '(("LEAF" (0 4)))))
    (assert (equal (flyparse-tree-info-list tree) '(0 20))))
  
  ;; Test tree mutators
  (let* ((tree
	  '("ROOT" (0 20)
	    ("LEAF" (0 4)))))
    (flyparse-set-tree-beg-offset tree 3)
    (flyparse-set-tree-end-offset tree 5)
    (assert (equal (flyparse-tree-beg-offset tree) 3))
    (assert (equal (flyparse-tree-end-offset tree) 5)))
  
  ;; Test absolute-tree-copy
  (let* ((tree
	  '("ROOT" (0 20))
	  ))
    (assert (equal (flyparse-absolute-tree-copy tree 5) '("ROOT" (5 25)))))
  
  ;; Test tree sorting helper
  (let* ((trees
	  '(("C" (30 50)) ("D" (50 100)) ("B" (20 30)) ("A" (0 20))))
	 (result (flyparse-sort-trees-asc trees)))
    (assert (equal result '(("D" (50 100)) ("C" (30 50)) ("B" (20 30)) ("A" (0 20))))))
  
  ;; Test containing region
  (let* ((trees
	  '(("C" (30 50)) ("D" (50 100)) ("B" (20 30)) ("A" (0 20))))
	 (result (flyparse-smallest-region-containing trees)))
    (assert (equal result '(0 101))))
  
  ;; Test simple query for root
  (let* ((tree 
	  '("ROOT" (0 20)
	    ("LEAF" (0 4))
	    ("LONG_LEAF" (0 16))))
	 (result (flyparse-query-first '("ROOT") tree)))
    (assert (equal result '("ROOT" (0 20) 
			    ("LEAF" (0 4))
			    ("LONG_LEAF" (0 16))))))
  
  ;; Test simple query for all leafs
  (let* ((tree 
	  '("ROOT" (10 20) 
	    ("LEAF" (0 4))
	    ("LEAF" (3 6))))
	 (result (flyparse-query-all '("ROOT" "LEAF") tree)))
    (assert (equal result '(("LEAF" (10 14))("LEAF" (17 20))))))

  
  ;; Test query upon query
  (let* ((tree 
	  '("ROOT" (0 23)
	    ("LEAF" (0 3))
	    ("SUBTREE" (3 20)
	     ("MINI" (0 10))
	     ("MINI" (0 7)))))
	 (initial (flyparse-query-first '("ROOT" "SUBTREE") tree))
	 (result (flyparse-query-all '("SUBTREE" "MINI") initial)))
    (assert (equal result '(("MINI" (6 16))("MINI" (16 23))))))
  
  ;; Test query with complex qualifier
  (let* ((tree 
	  '("ROOT" (10 20)
	    ("LEAF" (0 4))
	    ("LEAF" (3 6))))
	 (result (flyparse-query-first '("ROOT" ("LEAF" (at 17))) tree)))
    (assert (equal result '("LEAF" (17 20)))))
  
  ;; Test query with complex qualifier and more segments
  (let* ((tree 
	  '("ROOT" (10 20)
	    ("LEAF" (0 4))
	    ("SUBTREE" (3 6) ("LEAF" (0 1)) ("LEAF" (0 2)))))
	 (result (flyparse-query-all '("ROOT" ("SUBTREE" (at 17)) "LEAF") tree)))
    (assert (equal result '(("LEAF" (17 18))("LEAF" (18 20))))))
  
  ;; Test query with sub-query
  (let* ((tree 
	  '("ROOT" (10 20)
	    ("LEAF" (0 4))
	    ("SUBTREE" (3 6) ("APPLE" (0 1)) ("LEAF" (0 2)))))
	 (result (flyparse-query-all '("ROOT" ("SUBTREE" (has ("SUBTREE" "APPLE"))) "LEAF") tree)))
    (assert (equal result '(("LEAF" (18 20))))))

  ;; Test query with simple subquery used as filter
  (let* ((tree 
	  '("ROOT" (10 20) 
	    ("LEAF" (0 4))
	    ("LEAF" (3 6))
	    ("DUDE" (3 6))
	    ))
	 (result (flyparse-query-all '("ROOT" ("LEAF" (has ("LEAF")))) tree)))
    (assert (equal result '(("LEAF" (10 14))("LEAF" (17 20))))))


  ;; Test query with simple subquery used as a negation filter
  (let* ((tree 
	  '("ROOT" (10 20) 
	    ("LEAF" (0 4))
	    ("LEAF" (3 6)
	     ("DUDE" (0 3)))
	    ))
	 (result (flyparse-query-all '("ROOT" ("LEAF" (has-none ("LEAF" "DUDE")))) tree)))
    (assert (equal result '(("LEAF" (10 14))))))

  ;; Test query with 'text-match' complex option
  (let* ((tree 
	  '("ROOT" (10 25) 
	    ("LEAF" (0 4))
	    ("LEAF" (3 11) ("A" (0 1))("DUDE" (0 4)))
	    ))
	 (result (flyparse-query-all '("ROOT" ("LEAF" (text-match "ADUDE"))) tree)))
    (assert (=  1 (length result))))

  
  ;; Test query with universal qualifier
  (let* ((tree 
	  '("ROOT" (10 20)
	    ("LEAF" (0 4))
	    ("SUBTREE" (3 6) ("APPLE" (0 1)) ("LEAF" (0 2)))))
	 (result (flyparse-query-all '("ROOT" "SUBTREE" *) tree)))
    (assert (equal result '(("APPLE" (17 18))("LEAF" (18 20))))))
  
  
  ;; Test has-subtree-of-type-p
  (let* ((tree 
	  '("ROOT" (0 23)
	    ("LEAF" (0 3))
	    ("SUBTREE" (3 20)
	     ("MINI" (0 10))
	     ("MINI" (0 7))))))
    (assert (flyparse-has-subtree-of-type-p tree "LEAF"))
    (assert (flyparse-has-subtree-of-type-p tree "ROOT"))
    (assert (flyparse-has-subtree-of-type-p tree "MINI"))
    (assert (null (flyparse-has-subtree-of-type-p tree "APE"))))
  
  ;; Test containing-tree-of-type
  (let* ((tree 
	  '("ROOT" (0 23)
	    ("LEAF" (0 3))
	    ("SUBTREE" (3 20)
	     ("MINI" (0 10))
	     ("MINI" (0 7))))))
    (assert (equal (flyparse-containing-tree-of-type "MINI" 17 tree) '("MINI" (16 23))))
    (assert (equal (flyparse-containing-tree-of-type "MINI" 23 tree) '("MINI" (16 23))))
    (assert (equal (flyparse-containing-tree-of-type "MINI" 6 tree) '("MINI" (6 16))))
    (assert (null (flyparse-containing-tree-of-type "DOG" 6 tree)))
    (assert (null (flyparse-containing-tree-of-type "ROOT" 100 tree))))
  
  
  ;; Test basic, exaustive search
  (let* ((tree 
	  '("ROOT" (0 23)
	    ("LEAF" (0 3))
	    ("SUBTREE" (3 20)
	     ("MINI" (0 10))
	     ("MINI" (0 7)))))
	 (result (flyparse-search '("MINI") tree)))
    (assert (equal result '("MINI" (6 16)))))
  
  ;; Test basic, exaustive search with subquery
  (let* ((tree 
	  '("ROOT" (0 23)
	    ("LEAF" (0 3))
	    ("SUBTREE" (3 20)
	     ("MINI" (0 10))
	     ("MINI" (0 7)))))
	 (result (flyparse-search '("SUBTREE" (has ("SUBTREE" "MINI"))) tree)))
    (assert (equal result '("SUBTREE" (6 23) ("MINI" (0 10))("MINI" (0 7))))))
  
  
  ;; Test directed-search with subquery
  (let* ((tree 
	  '("ROOT" (0 23)
	    ("LEAF" (0 3))
	    ("SUBTREE" (3 20)
	     ("MINI" (0 10))
	     ("MINI" (0 7)))))
	 (result (flyparse-directed-search '("SUBTREE" (has ("SUBTREE" "MINI"))) 17 tree)))
    (assert (equal result '("SUBTREE" (6 23) ("MINI" (0 10))("MINI" (0 7))))))
  
  ;; Test directed-search-containing-region
  (let* ((tree 
	  '("ROOT" (0 23)
	    ("LEAF" (0 3))
	    ("SUBTREE" (3 20)
	     ("MINI" (0 10))
	     ("MINI" (0 7)))))
	 (result-a (flyparse-directed-search-containing-region '("SUBTREE") 7 20 tree))
	 (result-b (flyparse-directed-search-containing-region '("SUBTREE") 2 20 tree))
	 (result-c (flyparse-directed-search-containing-region '("ROOT") 2 20 tree)))
    (assert (equal result-a '("SUBTREE" (6 23) ("MINI" (0 10))("MINI" (0 7)))))
    (assert (null result-b))
    (assert (not (null result-c))))
  
  ;; Test flyparse-directed-search-smallest-containing-region
  (let* ((tree 
	  '("ROOT" (0 23)
	    ("LEAF" (0 3))
	    ("SUBTREE" (3 20)
	     ("MINI" (0 10))
	     ("MINI" (0 7)))))
	 (result-a (flyparse-directed-search-smallest-containing-region  7 20 tree))
	 (result-b (flyparse-directed-search-smallest-containing-region  0 23 tree))
	 (result-c (flyparse-directed-search-smallest-containing-region  0 40 tree)))
    (assert (equal result-a '("SUBTREE" (6 23) ("MINI" (0 10))("MINI" (0 7)))))
    (assert (equal result-b '("ROOT" (0 23)
			      ("LEAF" (0 3))
			      ("SUBTREE" (3 20)
			       ("MINI" (0 10))
			       ("MINI" (0 7))))))
    (assert (null result-c)))
  
  ;; Test flyparse-subtrees-contained-in-region
  (let* ((tree 
	  '("ROOT" (0 23)
	    ("LEAF" (0 3))
	    ("SUBTREE" (3 20)
	     ("MINI" (0 10))
	     ("MINI" (0 7)))))
	 (result-a (flyparse-subtrees-contained-in-region 0 23 tree))
	 (result-b (flyparse-subtrees-contained-in-region 1 23 tree)))
    (assert (equal result-a '(("LEAF" (0 3)) ("SUBTREE" (6 23) ("MINI" (0 10)) ("MINI" (0 7))))))
    (assert (equal result-b '(("SUBTREE" (6 23) ("MINI" (0 10)) ("MINI" (0 7)))))))
  
  ;; simple tree-as-text
  (let* ((tree 
	  '("IDENT" (0 5)
	    ("apple" (0 5)))))
    (assert (equal "apple" (flyparse-tree-as-text tree))))
  
  ;; bit more complex tree-as-text
  (let* ((tree 
	  '("IDENT" (0 20)
	    ("hello" (0 5))  
	    ("there" (2 7))  
	    ("mr" (2 4))  
	    ("apple" (2 7)))))
    (assert (equal "hello there mr apple" (flyparse-tree-as-text tree))))

  ;; bit more complex tree-as-text
  (let* ((tree '("JabberRegistrationHelper" (0 23))))
    (assert (equal "JabberRegistrationHelper" (flyparse-tree-as-text tree))))

  ;; bit more complex tree-as-text
  (let* ((tree '("NAME" (678 701) ("JabberRegistrationHelper" (0 23)))))
    (assert (equal "JabberRegistrationHelper" (flyparse-tree-as-text tree))))
  
  ;; nested tree-as-text
  (let* ((tree 
	  '("GREETING" (0 20)
	    ("COMPOUND" (0 5)
	     ("hi" (0 2))
	     ("ho" (2 4)))
	    ("there" (2 7))  
	    ("mr" (2 4))  
	    ("apple" (2 7)))))
    (assert (equal "hi ho there mr apple" (flyparse-tree-as-text tree))))
  
  ;; test simple cache operations
  (let* ((flyparse-tree-cache ;; Shadow global variable
	  (make-hash-table :test 'equal)))
    (flyparse-cache-tree '("A" (0 0)) "c:/a.c")
    (flyparse-cache-tree '("BAT" (0 2)) "c:/bat.c")
    (assert (equal '("A" (0 0)) (flyparse-get-cached-tree "c:/a.c")))
    (assert (equal '("BAT" (0 2)) (flyparse-get-cached-tree "c:/bat.c"))))
  
  (message "All tests passed :)") 
  )

;;;;;;;;;;;;;;;;;;;;;
;; testing helpers ;;
;;;;;;;;;;;;;;;;;;;;;


(defun flyparse-tree-for-string (cmd-list str)
  "A testing helper that makes a blocking call to 'cmd-list'
   and immediately returns the resultant parse-tree (with extraneous symbols
   stripped)."
  ;; e.g. (flyparse-tree-for-string (flyparse-cmd-for-file-type "aemon.as") "package aemon{class Dude{}}")
  (let* ((temp-file-name ".temp-file-for-testing-flyparse"))
    (with-temp-buffer
      (insert str)
      (let ((buffer-file-coding-system 'unix))
	(write-region (point-min) (point-max) temp-file-name nil 566))
      (let ((tree (flyparse-block-and-load-tree-from-file temp-file-name cmd-list)))
	(when (file-exists-p temp-file-name)
	  (delete-file temp-file-name))
	tree))))

;;;;;;;;;;;;;;;;;;;;;
;; general helpers ;;
;;;;;;;;;;;;;;;;;;;;;

(defun flyparse-block-and-load-tree-from-file (file-path cmd-list)
  "Parse and return the tree for the source file at 'file-path'. This is 
   a blocking call. Return nil on error of any kind."
  (let ((cmd (mapconcat 'identity (append cmd-list (list file-path)) " ")))
    (let* ((result-string (with-output-to-string
			    (with-current-buffer
				standard-output
			      (call-process shell-file-name nil 
					    '(t nil) ;; Keep stdout and throw away stderr
					    nil shell-command-switch cmd))))
	   (result-form (read result-string))
	   (tree '()))
      (if (listp result-form)
	  (progn
	    (eval result-form)
	    tree)))))

(defun flyparse-async-load-tree-from-file (file-path cmd-list callback)
  "Parse the source file at 'file-path' asynchronously, generate a parse-tree 
   and then invoke 'callback' with tree as argument."
  (lexical-let ((proc (apply 'start-process "*flyparse-proc*" nil
			     (first cmd-list)
			     (append (rest cmd-list) (list file-path))))
		(callback callback))
    (process-put proc 'parser-output "")
    (set-process-sentinel
     proc (lambda (process event) 
	    (let* ((result-string (process-get process 'parser-output))
		   (result-form (read result-string))
		   (tree '()))
	      (if (listp result-form)
		  (progn
		    (eval result-form)
		    (funcall callback tree))))))
    (set-process-filter
     proc (lambda (process output)
	    (let ((source-buffer (process-buffer process))
		  (parser-output-so-far (process-get process 'parser-output)))
	      (process-put process 'parser-output (concat parser-output-so-far output)))))))

(defun flyparse-walk-path (dir action)
  "Walk 'dir' recursively, executing 'action' with (dir file) for each file."
  (cond ((file-directory-p dir)
	 (or (char-equal ?/ (aref dir(1- (length dir))))
	     (setq dir (file-name-as-directory dir)))
	 (let ((files (directory-files dir nil nil t)))
	   (dolist (file files)
	     (cond ((member file '("." ".." ".svn")))
		   (t
		    (let ((fullname (concat dir file)))
		      (funcall action dir file)
		      (if (file-directory-p fullname)
			  (flyparse-walk-path fullname action))))))))
	(t
	 (funcall action
		  (file-name-directory dir)
		  (file-name-nondirectory dir)))))


(defun flyparse-re-search-containing-point (regex limit-start limit-end group-number-containing-point pos)
  "A helper for finding regex matches for which the current point is contained in a specified group."
  (save-excursion
    (goto-char limit-end)
    (catch 'return-now
      (while (> (point) limit-start)
	(let* ((search-result (re-search-backward regex limit-start 1))
	       (contains-result (and search-result
				     (<= (match-beginning group-number-containing-point) pos)
				     (>= (match-end group-number-containing-point) pos))))

	  (cond ((and search-result (not contains-result))
		 (goto-char (- (match-end 0) 1)))

		((and search-result contains-result)
		 (throw 'return-now t)))
	  )))))


(provide 'flyparse-mode)

