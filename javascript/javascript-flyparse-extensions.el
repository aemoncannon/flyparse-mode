(require 'flyparse-mode)

(defvar javascript-flyparse-parse-cmd 
  '("java" "emacs.flyparse.javascript.JavascriptDriver"))

(add-hook 'javascript-mode-hook
          (lambda ()
	    (setq flyparse-parse-cmd javascript-flyparse-parse-cmd)
	    (flyparse-mode)
	    ))

(provide 'javascript-flyparse-extensions)