(defpackage :netersys.web
  (:use :cl :net.aserve :netersys.utils)
  (:import-from :cl+ssl
		:make-ssl-server-stream)
  (:export :start-netersys
	   :stop-netersys
	   :*current-server*))

(in-package :netersys.web)

(defvar *domain* "netersys.com")
(defvar *domain-ip* "172.105.81.235")
(defvar *current-server* nil)

(defparameter *cert*
  (merge-pathnames #P"certs/fullchain.pem" *static-dir*))
(defparameter *privkey*
  (merge-pathnames #P"certs/privkey.pem" *static-dir*))

;;; Start a socket server
(defun start-netersys (&key (port 8080) (cert *cert*)
			 (privkey *privkey*))
  (setf *current-server*
	(start :host *domain* :port port
	       :accept-hook #'(lambda (socket)
				(cl+ssl:make-ssl-server-stream
				 socket
				 :external-format '(:iso-8859-1
						    :eol-style :crlf)
				 :certificate (namestring cert)
				 :key (namestring privkey))))))

(defun stop-netersys ()
  (shutdown :server *current-server*))
