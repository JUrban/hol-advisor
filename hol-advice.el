;; Asynchronous code talking to the remote HOL Light proof advisor
;; (mostly modified from mizar.el).
;;
;; SYNOPSIS: M-x hol-ask-advisor RET ~F ==> T RET
;;
;; Use also ";;;" after a goal, or C-ca on a goal 
;; (when inside the hol-light mode)
;;
;; License:     GPL (GNU GENERAL PUBLIC LICENSE) version 2 or later
;; Contact:     Josef Urban (Josef dot Urban at gmail dot com) 
;;
;;
;; (defgroup hol-light nil
;;   "Major mode for authoring HOL articles"
;;   :group 'languages)

(require 'cl)

(defgroup hol-proof-advisor nil
  "HOL Proof Advisor settings"
  :group 'hol-light)

(defcustom hol-advisor-server "colo12-c703.uibk.ac.at" ;"mizar.cs.ualberta.ca"; 
"Server for the HOL Proof Advisor."
:type 'string
:group 'hol-proof-advisor)

(defcustom hol-advisor-port 8080
"Port where the HOL Proof Advisor runs."
:type 'integer
:group 'hol-proof-advisor)

(defcustom hol-atp-completion t
"Triple semicolon calls ATP to provide justification."
:type 'boolean
:group 'hol-proof-advisor)

(defcustom hol-atp-desync-limit 1000
"Character extent where we try to synchronize ATP output with original text."
:type 'integer
:group 'hol-proof-advisor)


(defcustom hol-atp-cgi  "/hh/cache.php"  ;"/~mptp/cgi-bin/MizAR.cgi";
"Path to the Hol ATP CGI script on `hol-atp-server'."
:type 'string
:group 'hol-proof-advisor)


(defcustom hol-current-project "Flyspeck"
"The name of the project that we work on. 
Needed to have persistance on the server."
:type 'string
:group 'hol-proof-advisor)

(defcustom hol-query-timelimit 100
"Maximum time for which we let one query be repeatedly asked to HH server."
:type 'integer
:group 'hol-proof-advisor)


(defun hol-semicolon (&optional arg)
  "Call ATP on ;;;"
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

(defun hol-fla-at-point ()
  "Return the backquoted fla at the point."
  (save-excursion
    (skip-chars-backward "^`")
    (if (looking-at "\\([^`]+\\)")
	(buffer-substring-no-properties (match-beginning 1) (match-end 1))
      "No HOL formula at point")))

(defun hol-mark-call-atp (beg end)
"Mark the region with the 'help-echo and 'atp-asked property and call ATP with the preceding formula."
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


(defun insert-atp-result (buf holpos atpres)
"Try to find text with property 'atp-asked set to HOLPOS and replace with ATPRES."
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
"Sentinel for the HOL Advisor"
;  (message (prin1-to-string (process-status process)))
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


(defun hol-ask-advisor (&optional thm returnpos)
  "Send THM to the HOL Advisor.
Resulting advice is shown as a message or inserted at the RETURNPOS ."
  (interactive)
;  (interactive "sconjecture: ")
  (let* ((thm 
	  (or thm (read-string  (concat "goal (default " (hol-fla-at-point) " ): " )
				nil nil      (hol-fla-at-point))))
	 (request (concat thm "\n"))
	 (bname (if returnpos (concat "*advpos::" returnpos "*")
		  (concat "*noadvpos::" (symbol-name (gensym)) "*")))
	 (aproc (open-network-stream "GetTCP" bname hol-advisor-server hol-advisor-port)))
    (set-process-sentinel aproc 'hol-atp-sentinel)
    (process-send-string aproc request)
;    (if (not returnpos) (display-buffer bname))
    ))


(define-key hol-light-mode-map "\C-ca" 'hol-ask-advisor)
(define-key hol-light-mode-map ";" 'hol-semicolon)



(defun hol-atp-autocomplete1 ()
"Replace \";;;\" with \";; (* ATP asked ... *)\" and call ATP to justify the current step.
Used automatically if `hol-atp-completion' is on."
(interactive)
(save-excursion
  (let* ((pos (point)) (pos1 (- pos 3)))
    (forward-char -3) 
    (if (looking-at ";;;")
	(progn 
	  (replace-match ";; (* ATP asked ... *)")
	  ;; We leave one space in the beg and end without the added properties
	  ;; not to get sticky behavior for unsuspecting users.
	  ;; Problem is that the hl error position is right after the formula
	  (hol-mark-call-atp1 pos1 (- (point) 1)))))))


;; frontend

(defun hol-mark-call-atp1 (beg end)
"Mark the region with the 'help-echo and 'atp-asked property and call ATP with pushback data."
(save-excursion
  (goto-char beg)
  (let* ((mod (buffer-modified-p))
	 (pos (number-to-string beg))
	 (buf (buffer-name))
	 (msg (concat "ATP was called on this step, awaiting response for position " pos)))
    (skip-chars-backward "^`")
    (forward-char -2)
    (hol-remote-solve-atp (hol-fla-at-point) buf pos)
;    (hol-remote-solve-atp "Positions" miz-pos (concat buf "__" pos) (list buf line col beg))
    (put-text-property beg end 'help-echo msg)
    (put-text-property beg end 'atp-asked (intern pos))
    (message "Calling ATP on position %s " pos)
    (set-buffer-modified-p mod))))


(defun hol-remote-solve-atp (&optional fla output-buffer position)
"Send the current article to a remote server for verification and
ask a remote ATP for solving of all Hol-unsolved problems.
Calls `hol-remote-solve'.
"
(interactive "*P")
(hh-url-get-loop hol-current-project fla 'hh-check-url-buffer hol-query-timelimit output-buffer position))

(defcustom hh-request-gap 2
"The waiting time between repeated requests sent to the HH
server checking if the query is finished."
:type 'integer
:group 'hol-proof-advisor)

;; e.g.:  (hh-url-get-loop "Ramsey" "EVEN x \\/ EVEN (x + 5)" 'hh-check-url-buffer 20 "bla.hl" 300)
(defun hh-url-get-loop (session query pred tl0 buf pos)
"Ask the HH server SESSION a QUERY with a time limit TL0.
Repeat the query until (eval PRED) is true in the request buffer,
or when the time is up. Put the answer into buffer BUF."
(let ((uss url-show-status)
      (tl (+ (float-time) tl0))
      (arg-stuff (concat "?s=" (url-hexify-string session)
			 "&q=" (url-hexify-string query))))
  (setq url-show-status nil)
  (hh-url-get-loop-intern (concat "http://" hol-advisor-server hol-atp-cgi arg-stuff) pred tl buf pos uss)))

(defun hh-url-get-loop-intern (url pred tl buf pos uss)
      (let ((url-request-method "GET") (pred pred) (tl tl))
        (url-retrieve url 'hh-check-url-buffer-loop (list buf pos url pred tl uss))))

(defun hh-check-url-buffer-loop (status &optional buf pos url pred tl uss)
(let ((res (funcall pred)))
  (cond 
   (res
    (setq url-show-status uss)
    (insert-atp-result buf pos (cadr res)))
   ((> (float-time) tl)
    (setq url-show-status uss)
    (insert-atp-result buf pos "Time limit exceeded"))
   (t
    (run-with-timer hh-request-gap nil 'hh-url-get-loop-intern url pred tl buf pos uss)))))


(defun hh-check-url-buffer ()
"Check the result buffer of a HH request for a finished answer.
If the request is finished, return the result, otherwise return
nil."
; (message "%s" (buffer-name))
(goto-char (point-max))
(skip-chars-backward "^`")
(let ((res
       (cond 
	((eq (point) (point-min)) nil) ; return nil here - not complete yet
	((looking-back "SUCCESS[^:]*:\\([^`]+\\)`")
	 (list "S" (buffer-substring-no-properties (match-beginning 1) (match-end 1))))
	((looking-back "Minimized[^:]*:\\([^`]+\\)`")
	 (list "M" (buffer-substring-no-properties (match-beginning 1) (match-end 1))))
	((looking-back "Result[^:]*:\\([^`]+\\)`")
	 (list "R" (buffer-substring-no-properties (match-beginning 1) (match-end 1))))
	((looking-back "No more advice`")
	 (list "F" "No more advice"))
	((looking-back "\\(.*\\)`") ; fallback - unknown stuff
	 (list "U" (buffer-substring-no-properties (match-beginning 1) (match-end 1))))
	(t (list "U" "Unknown"))))) ; safety
      (if res (message "HH returned %s" (cadr res)))
      res))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; not used now


(defun get-tcp-response (bufname host request &optional port)
  "Fetch TCP REQUEST from HOST on PORT, put result into buffer BUFNAME and return it.
Previous contents of BUFNAME is deleted. This is synchronous and may hang."
  (if (get-buffer bufname) (kill-buffer bufname))
  (let* ((port (or port hol-advisor-port))
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


;; borrowed from somewhere - http-post code
;; the problem is to pass things to a browser
(defun my-url-http-post (url args &optional output-buffer synchronous pushback)
      "Send ARGS to URL as a POST request."
      (let ((url-request-method "GET") ;"POST")
            (url-request-extra-headers
             '(("Content-Type" . "application/x-www-form-urlencoded")))
            (url-request-data
             (mapconcat (lambda (arg)
                          (concat (url-hexify-string (car arg))
                                  "="
                                  (url-hexify-string (cdr arg))))
                        args
                        "&")))
        ;; if you want, replace `my-switch-to-url-buffer' with `my-kill-url-buffer'
	(if synchronous
	    (save-excursion
	      (set-buffer (url-retrieve-synchronously url))
	      (buffer-string))
        (url-retrieve url 'my-switch-to-url-buffer (list output-buffer pushback)))))


(defun my-switch-to-url-buffer (status &optional output-buffer pushback)
  "Switch to buffer returned by `url-retreive', rename it to OUTPUT-BUFFER or *atp-output*.
   If PUSHBACK is given, it must be a list (buffer-name line column position) telling
   where to insert the result.
    The buffer contains the raw HTTP response sent by the server."
  (or pushback (switch-to-buffer-other-window (current-buffer)))
  (let ((bufname (or output-buffer "*atp-output*")))
     (if (get-buffer bufname) (kill-buffer bufname))
     (rename-buffer bufname)
     (add-to-invisibility-spec 'miz-ar4miz-invis)
     (make-variable-buffer-local 'line-move-ignore-invisible)
     (setq line-move-ignore-invisible t)
     (unless pushback
	 ;; put ATP result into the mizar buffer
	 (save-excursion
	   (goto-char (point-min))
	   (let* ((mizbuf (car pushback)) (line (cadr pushback)) 
		  (col (third pushback)) (mizpoint (fourth pushback))
		  (colstr (int-to-string col)) (colstr1 (int-to-string (- col 1))) 
		  (linestr (int-to-string line)) (atppos (concat linestr "_" colstr1)) 
		  (mizpos (concat linestr ":" colstr))
		  (regpos (concat atppos ":::\\(..*\\)"))
		  (allrefs nil) (atpres "ATP-Unsolved"))
	     (while (re-search-forward regpos (point-max) t)
	       (setq allrefs (nconc allrefs (split-string (match-string 1) ","))))
	     (setq allrefs (unique allrefs))	     
	     (if allrefs (setq atpres (mapconcat 'identity allrefs ",")))
	     (insert-atp-result mizbuf line col mizpoint mizpos atpres allrefs bufname)
	     (message "ATP answered for position %s with %s" mizpos atpres))))
))
;     (setup-atp-output-invisibility)))


;; simple query for debug
(defun fbquery (session query)
      (let ((url-request-method "GET")
            (arg-stuff (concat "?s=" (url-hexify-string session)
                         "&q=" (url-hexify-string query))))
        (url-retrieve (concat "http://" hol-advisor-server hol-atp-cgi arg-stuff)
                      (lambda (status) (switch-to-buffer (current-buffer))))))
