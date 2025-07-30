;; read http url from emacs and display in buffer

(let* ((url "http://localhost:4000/api/hello")
       (buf (url-retrieve-synchronously url t t 10))
       body)
  (unless buf (error "failed %s" url))
  (with-current-buffer buf
    (goto-char (point-min))
    (re-search-forward "^$" nil 'move)
    (setq body (buffer-substring-no-properties (point) (point-max)))
    (kill-buffer))
  (with-current-buffer (get-buffer-create "*hello*")
    (erase-buffer)
    (insert body)
    (display-buffer (current-buffer))))

;; get json using request
(use-package request)
(require 'request)

(request "http://localhost:4000/api/hello"
  :parser 'json-read
  :encoding 'utf-8-lang
  :success (cl-function
	    (lambda (&key data &allow-other-keys)
	      (let ((buf (get-buffer-create "*hello*")))
		(with-current-buffer buf
		  (erase-buffer)
		  (insert (json-encode data))
		  (json-pretty-print-buffer))
		(display-buffer buf))))
  :error (cl-function
	  (lambda (&rest _)
	    (message "failed"))))

