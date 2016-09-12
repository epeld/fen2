
(defclass attribute ()
    ())

(defgeneric apply-attribute (t attribute))


;; 
;; Title
;; 

(defclass title (attribute)
  ((title-string :type 'string
		 :initarg :title)))


(defmethod apply-attribute ((a title) c)
  (jcall "setTitle" c (slot-value a 'title-string)))


(defun title (name)
  (make-instance 'title 
		 :title name))


;; 
;; Size
;; 

(defclass size (attribute)
  ((width :type 'integer
	  :initarg :width)
   (height :type 'integer
	   :initarg :height)))


(defmethod apply-attribute ((a size) c)
  (jcall "setSize" c 
	 (slot-value a 'width)
	 (slot-value a 'height)))


(defun size (width height)
  (make-instance 'size 
		 :width width
		 :height height))

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
;; Text
;; 
(defclass text (attribute)
  ((text :type 'string
	 :initarg :text)))


(defmethod apply-attribute ((a text) c)
  (jcall "setText" c (slot-value a 'text)))


(defun text (text)
  (make-instance 'text 
		 :text text))

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


