;; Simple synchronous advising code stolen from my mizar.el 
;;
;; SYNOPSIS: M-x hol-ask-advisor RET ~F ==> T RET
;;
;; TODO: steal also the asynchronous used for MizAR



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

(defvar advisor-output "*Proof Advice*")

(defgroup hol nil
  "Major mode for authoring HOL articles"
  :group 'languages)

(defgroup hol-proof-advisor nil
  "HOL Proof Advisor settings"
  :group 'hol)

(defcustom advisor-server "colo12-c703.uibk.ac.at"
"Server for the HOL Proof Advisor."
:type 'string
:group 'hol-proof-advisor)

(defun hol-ask-advisor (thm)
  "Send THM to the HOL Advisor.
Resulting advice is shown in the buffer *Proof Advice*."
  (interactive "sconjecture: ")
  (let* ((request (concat thm "\n"))
	 (abuffer (get-tcp-response advisor-output advisor-server request)))
    (if abuffer
	(switch-to-buffer-other-window abuffer)	       
      (message "No references advised"))
    )
  )
