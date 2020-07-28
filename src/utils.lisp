(defpackage :netersys.utils
  (:use :cl :net.aserve :cl+ssl)
  (:export :*prj-dir*
	   :*src-dir*
	   :*static-dir*
	   :*imgs-dir*
	   :*css-dir*
	   :*js-dir*
	   :*dir-match*
	   :link
	   :path
	   :img
	   :css
	   :js
	   :stop-netersys))

(in-package :netersys.utils)

(defvar *prj-dir* (asdf:component-pathname
		   (asdf:find-system :netersys)))
;;(setf *project-dir* *prj-dir*)
(defvar *src-dir* (merge-pathnames #P"src/" *prj-dir*))
(defvar *static-dir* (merge-pathnames #P"static/" *prj-dir*))
;;(setf *static-dir* *stic-dir*)
(defvar *imgs-dir* (merge-pathnames #P"imgs/" *static-dir*))
(defvar *css-dir* (merge-pathnames #P"css/" *static-dir*))
(defvar *js-dir* (merge-pathnames #P"js/" *static-dir*))

(defvar *dir-match* `((img . ,*imgs-dir*)
		      (js . ,*js-dir*)
		      (css . ,*css-dir*)))

(defun link (&optional (where 'loc) (sublink nil) (ac nil))
  (concatenate 'string
	       (if (eq where 'loc)
		   (concatenate 'string *host-name*
				(if sublink
				    (concatenate 'string "/" sublink
						 (if ac
						     (concatenate 'string "#" ac)
						     ""))
				    ""))
		   (if sublink
		       sublink
		       (error "A valid URL cannot be obtained from the given arguments")))))

(defun path (where filename)
  (let ((root (cdr (assoc where *dir-match*))))
    (print root)
    (merge-pathnames filename root)))

(defmacro img (filename)
  `(path 'img ,filename))
(defmacro css (filename)
  `(path 'css ,filename))
(defmacro js (filename)
  `(path 'js ,filename))
