
(defclass attribute ()
    ())

(defgeneric apply-attribute (t attribute))

;; 
;; Macro
;; 

(defmacro defattr (name args &body body)
  (let* ((inst (gensym))
	 (setters (loop for arg in args collect
		      `(setf (slot-value ,inst ',arg)
			     ,arg))))
    `(progn
       (defclass ,name (attribute)
	 ,(mapcar #'list args))
     
       (defun ,name ,args
	 (let ((,inst (make-instance ',name)))
	   ,@setters
	   ,inst))
     
       (defmethod apply-attribute ((,inst ,name) component)
	 (with-slots ,args ,inst
	   ,@body)))))


;; 
;; Simple Attributes
;; 

(defattr title (title-string)
  (set-title component title-string))

(defattr size (width height)
  (set-size component width height))

(defattr text (text)
  (set-text component text))

(defattr color (color)
  (set-foreground-color component (string-color color)))

(defattr background-color (color)
  (set-background-color component (string-color color)))

(defattr opaque (bool)
  (set-opaque component bool))

;; 
;; Children
;;  

(defclass children (attribute)
  ((children :type 'list
	     :initarg :children)))


(defun add-child (component child)
  (jcall "add" component child))


(defmethod apply-attribute ((a children) c)
  (dolist (child (slot-value a 'children))
    (add-child c child)))


(defmacro children (&rest forms)
  `(make-instance 'children
		  :children (list ,@forms)))


;; 
;; Layout
;; 

(defvar page-axis
  (jfield "javax.swing.BoxLayout" "PAGE_AXIS"))

(defvar line-axis
  (jfield "javax.swing.BoxLayout" "LINE_AXIS"))

(defvar x-axis
  (jfield "javax.swing.BoxLayout" "X_AXIS"))

(defvar y-axis
  (jfield "javax.swing.BoxLayout" "Y_AXIS"))


(defclass layout (attribute)
  ((layout :type :keyword
	   :initarg :layout)
   (axis :initarg :axis)))


(defun box-layout (c axis)
  (jnew "javax.swing.BoxLayout" c axis))


(defun flow-layout ()
  (jnew "java.awt.FlowLayout"))


(defun make-layout (a c)
  (case (slot-value a 'layout)
    (:box (box-layout c (slot-value a 'axis)))
    (:flow (flow-layout))))


(defmethod apply-attribute ((a layout) c)
  (let ((pane (jcall "getContentPane" c)))
    (set-layout pane (make-layout a pane))))


(defun set-layout (c layout)
  (jcall "setLayout" c layout))


(defun layout (value &optional axis)
  (the keyword value)
  (let ((lt (make-instance 'layout 
			   :layout value)))
    (setf (slot-value lt 'axis)
	  (or axis y-axis))
    lt))
