;;; -*- mode: lisp -*-

(defpackage :netersys-app
  (:use :cl :asdf))
(in-package :netersys-app)

(defsystem netersys
  :author "Arnold N'GORAN"
  :licence "LLGPL"
  :components
  ((:module "src"
	    :components ((:file "utils")
			 (:file "web"
				:depends-on ("utils"))
			 (:file "netersys"
				:depends-on ("utils" "web")))))
  :depends-on (port cl-who cl-serve))
