;; Simple synchronous advising code stolen from my mizar.el 
;;
;; SYNOPSIS: M-x hol-ask-advisor RET ~F ==> T RET
;;
;; TODO: steal also the asynchronous used for MizAR - DONE now, use ";;;" or C-ca or `hol-ask-advisor'


;; (defgroup hol-light nil
;;   "Major mode for authoring HOL articles"
;;   :group 'languages)

(defgroup hol-proof-advisor nil
  "HOL Proof Advisor settings"
  :group 'hol-light)

(defvar advisor-output "*Proof Advice*")

(defcustom advisor-server "colo12-c703.uibk.ac.at"
"Server for the HOL Proof Advisor."
:type 'string
:group 'hol-proof-advisor)


(defcustom hol-atp-completion t
"*Double semicolon calls ATP to provide justification."
:type 'boolean
:group 'hol-proof-advisor)

(defun hol-semicolon (&optional arg)
  "Call ATP on ;;"
  (interactive "*p")
  (self-insert-command (prefix-numeric-value arg))
  (if (and hol-atp-completion
	   (looking-back ";;;" (- (point) 3)))
      (hol-atp-autocomplete)))


(defun hol-atp-autocomplete ()
"Replace \";;;\" with \";; (* ATP asked ... *)\" and call ATP to justify the current step.
Used automatically if `hol-atp-completion' is on."
(save-excursion
  (let* ((pos (point)) (pos1 (- pos 3)))
    (forward-char -3) 
    (if (looking-at ";;;")
	(progn 
	  (replace-match ";; (* ATP asked ... *)")
	  ;; We leave one space in the beg and end without the added properties
	  ;; not to get sticky behavior for unsuspecting users.
	  ;; Problem is that the hl error position is right after the formula
	  (hol-mark-call-atp pos1 (- (point) 1)))))))

(defun hol-mark-call-atp (beg end)
"Mark the region with the 'help-echo and 'atp-asked property and call ATP with pushback data."
(save-excursion
  (goto-char beg)
  (let* ((mod (buffer-modified-p))
	 (pos (number-to-string beg))
	 (buf (buffer-name))
	 (msg (concat "ATP was called on this step, awaiting response for position " pos)))
    (skip-chars-backward "^`")
    (forward-char -2)
    (hol-ask-advisor (hol-fla-at-point) (concat buf ":::" pos))
    (put-text-property beg end 'help-echo msg)
    (put-text-property beg end 'atp-asked (intern pos))
    (message "Calling ATP on position %s " pos)
    (set-buffer-modified-p mod))))

(defun get-tcp-response (bufname host request &optional port)
  "Fetch TCP REQUEST from HOST on PORT, put result into buffer BUFNAME and return it.
Previous contents of BUFNAME is deleted. This is synchronous and may hang."
  (if (get-buffer bufname) (kill-buffer bufname))
  (let* ((port (or port 8080))
	 (proc (open-network-stream "GetTCP" bufname host port))
         (buf (process-buffer proc)))
    (process-send-string proc request)
    ;; Watch us spin and stop Emacs from doing anything else!
    (while (equal (process-status proc) 'open)
      (when (not (accept-process-output proc 180))
        (delete-process proc)
        (error "Network timeout!")))
    (delete-process proc)

    (unless (> (buffer-size buf) 0)
      (error "Unable to fetch %s from %s." request host))
    buf))


;; (defun hol-ask-advisor (thm)
;;   "Send THM to the HOL Advisor.
;; Resulting advice is shown in the buffer *Proof Advice*."
;;   (interactive "sconjecture: ")
;;   (let* ((request (concat thm "\n"))
;; 	 (abuffer (get-tcp-response advisor-output advisor-server request)))
;;     (if abuffer
;; 	(switch-to-buffer-other-window abuffer)	       
;;       (message "No references advised"))
;;     )
;;   )


(defun hol-fla-at-point ()
  "Return the backquoted fla at the point."
  (save-excursion
    (skip-chars-backward "^`")
    (if (looking-at "\\([^`]+\\)")
	(buffer-substring-no-properties (match-beginning 1) (match-end 1))
      "No HOL formula at point")))

(defcustom hol-atp-desync-limit 1000
"*Character extent where we try to synchronize ATP output with original text."
:type 'integer
:group 'hol-proof-advisor)

(defun insert-atp-result (buf holpos atpres)
"Try to find text with property 'atp-asked set to HOLPOS around HOLPOINT and replace with ATPRES."
(save-excursion
  (set-buffer buf)
  (let* ((holpoint (string-to-number holpos))
	 (start (max (point-min) (- holpoint hol-atp-desync-limit)))
	 (end (min (point-max) (+ holpoint (* 4 hol-atp-desync-limit))))
	 (pos1 (text-property-any start end 'atp-asked (intern holpos))))
    (if (not pos1) (message "Position for ATP solution of %s not found" holpos)
      (save-excursion
	(goto-char pos1)
	(if (not (looking-at ";; ([*] ATP asked ... [*])"))
	    (message "Position for ATP solution of %s user-edited. No inserting." holpos)
	  (replace-match atpres)))))))


(defun hol-atp-sentinel (process signal)
  (message (prin1-to-string (process-status process)))
  (if (memq (process-status process) '(closed exit signal))
      (save-excursion
	(set-buffer (process-buffer process))
	(let* ((bname (buffer-name))
	       (res (buffer-string)))
	  ; put the result into the buffer and position if we were asked
	  (if (string-match "^[*]advpos::\\(.*\\):::\\(.*\\)[*]$" bname)
	      (let ((retbuf (match-string 1 bname))
		    (retpos (match-string 2 bname))
		    (retstr
		     (if (string-match "Replaying: SUCCESS [^:]*: *\\(.*\\)" res)
			 (concat ";; e(" (match-string 1 res) ");;")
		       ";; (* No ATP proof found *)")
		     ))
		    (insert-atp-result retbuf retpos retstr)))
	  (message res))
;; comment for debugging
	(kill-buffer)
)))


(defun hol-ask-advisor (thm &optional returnpos)
  "Send THM to the HOL Advisor.
Resulting advice is shown as a message"
  (interactive "sconjecture: ")
  (let* ((request (concat thm "\n"))
	 (bname (if returnpos (concat "*advpos::" returnpos "*")  
		  (concat "*noadvpos::" (symbol-name (gensym)) "*")))  
	 (aproc (open-network-stream "GetTCP" bname  "colo12-c703.uibk.ac.at" "8080")))
    (set-process-sentinel aproc 'hol-atp-sentinel)
    (process-send-string aproc request)
    (if (not returnpos) (display-buffer bname))
))


(define-key hol-light-mode-map "\C-ca" 'hol-ask-advisor)
(define-key hol-light-mode-map ";" 'hol-semicolon)
