;;;; storage.lisp
;;;;
;;;; This file is part of the restas-colorize library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas.colorize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; generic storage interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric storage-count-pastes (storage))

(defgeneric storage-list-all-pastes (storage))

(defgeneric storage-list-pastes (storage offset limit))

(defgeneric storage-get-paste (storage id))

(defgeneric storage-add-paste (storage paste))

(defgeneric storage-remove-paste (storage id))

(defclass paste ()
  ((id :initarg :id :initform nil :accessor paste-id)
   (date :initarg :date :initform nil :accessor paste-date)
   (author :initarg :author :initform nil :accessor paste-author)
   (title :initarg :title :initform nil :accessor paste-title)
   (lang :initarg :lang :initform nil :accessor paste-lang)
   (code :initarg :code :initform nil :accessor paste-code)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; implementation storage in memory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass memory-storage ()
  ((pastes :initform nil)
   (last-id :initform 0)))

(defmethod storage-count-pastes ((storage memory-storage))
  (length (storage-list-all-pastes storage)))

(defmethod storage-list-all-pastes ((storage memory-storage))
  (slot-value storage 'pastes))

(defmethod storage-list-pastes ((storage memory-storage) offset limit)
  (let* ((pastes (storage-list-all-pastes storage))
         (len (length pastes))
         (end (+ limit offset)))
    (if (and (not (minusp offset))
             (> len offset))
        (subseq pastes
                offset
                (if (and pastes (< end len))
                    end)))))

(defmethod storage-get-paste ((storage memory-storage) id)
  (find id
        (slot-value storage 'pastes)
        :key #'paste-id))

(defmethod storage-add-paste ((storage memory-storage) paste)
  (setf (slot-value paste 'id)
        (incf (slot-value storage 'last-id)))
  (setf (slot-value paste 'date)
        (pretty-time (local-time:now)))
  (push paste
        (slot-value storage 'pastes))
  paste)

(defmethod storage-remove-paste ((storage memory-storage) id)
  (setf (slot-value storage 'pastes)
        (remove id
                (slot-value storage 'pastes)
                :key #'(lambda (paste) (getf paste :id)))))

(defparameter *storage-dir* (merge-pathnames "stor/"
					     (asdf:component-pathname 
					      (asdf:find-system '#:restas-colorize))))

(defparameter *storage-wildcard* (merge-pathnames "*" *storage-dir*))

(ensure-directories-exist *storage-dir*)

(defun id->path (id)
  (merge-pathnames (format nil "~a" id) *storage-dir*))

(defun path->id (path)
  (parse-integer (file-namestring path) :junk-allowed t))

(defun paste-file-ids ()
  (remove-if-not #'integerp (mapcar #'path->id
				  (directory *storage-wildcard*))))
(defun last-file-id ()
  (apply #'max (paste-file-ids)))

(defclass file-storage (memory-storage)
  ())

(defun load-paste (id)
  (with-open-file (fs (id->path id))
    (let ((saved (read fs)))
      (make-instance 'paste 
		     :id (getf saved :id)
		     :author (getf saved :author)
		     :lang (getf saved :lang)
		     :code (getf saved :code)
		     :title (getf saved :title)
		     :date (getf saved :date)))))

(defun store-paste (paste)
  (with-slots (id author lang code title date) paste
    (let ((saved (list :id id
		       :author author
		       :lang lang
		       :code code
		       :title title
		       :date date)))
      (with-open-file (fs (id->path (paste-id paste)) 
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
	(format fs "~S" saved)))))

(defgeneric storage-reset (storage))

(defmethod storage-reset ((storage file-storage))
  (setf (slot-value storage 'pastes) 
	nil)
  (mapc #'(lambda (id)
	    (push (load-paste id)
		  (slot-value storage 'pastes)))
	(paste-file-ids))
  (setf (slot-value storage 'last-id) (last-file-id)))
  

(defmethod initialize-instance :after ((storage file-storage) &key)
  (storage-reset storage))

(defmethod storage-add-paste :after ((storage file-storage) paste)
  (store-paste paste))

(defmethod storage-remove-paste :after ((storage file-storage) id))

(setf *storage*
      (make-instance 'file-storage))

