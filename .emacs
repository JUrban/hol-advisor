 (global-font-lock-mode t)
 (setq require-final-newline t)
 (setq load-path (cons (substitute-in-file-name "$HOME/hol-advisor") load-path))
(autoload 'hol-light-mode "hol-light" nil t)
(setq auto-mode-alist (append '( ("\\.ml$" . hol-light-mode) 
				 ("\\.hl$" . hol-light-mode))
			      auto-mode-alist))
(add-hook 'hol-light-load-hook #'(lambda () (require 'hol-advice)))
