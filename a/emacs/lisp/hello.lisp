
(ql:quickload '(:hunchentoot :simple-routes :cl-json)) ;; one time in repl

;; (ql:quickload :com.inuoe.jzon) 
;; (ql:quickload '(:jzon :cl-yaml)) 

;; (com.inuoe.jzon:stringify '(:message "hey"))

(defpackage #:hello
  (:use #:cl #:hunchentoot #:simple-routes))


;;  (:import-from #:com.inuoe.jzon #:stringify #:parse))

(in-package #:hello)

;; (stringify '(:message "hey"))
;; (parse "{\"m\":1}")
;; (let ((ht (make-hash-table :test #'equal)))
;;   (setf (gethash "m" ht) 1)
;;   (stringify ht))

(setf simple-routes:*routeslist*
      (simple-routes:compile-routes
       (:GET "/api/hello" 'hello-json)))

(defun hello-json ()
  (setf (hunchentoot:header-out :content-type hunchentoot:*reply*)
	"application/json")
  ;; (setf hunchentoot:content-type* "application/json")
  (cl-json:encode-json-to-string
   '(("m" . 1))))

;; (defvar *srv* (make-instance 'hunchentoot:easy-acceptor :port 4000))
(defvar *srv* (make-instance 'simple-routes:simpleroutes-acceptor :port 4000))
;;(hunchentoot:stop *srv*)
(hunchentoot:start *srv*)

