
(defclass attribute ()
    ((priority :documentation "The priority of this attribute"
	       :initform 5
	       :initarg :priority
	       :accessor priority
	       :allocation :class)))

(defgeneric apply-attribute (attribute t))

(defun apply-attributes (component attributes)
  "Apply a list of attributes to the component"
  (the list attributes)
  (let ((attributes (sort (copy-seq attributes) #'> :key #'priority)))
    (dolist (attr attributes)
      (apply-attribute attr component))))


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

(defattr aligned (keyword)
  (assert (member keyword (list :west :east :north :south)))
  (add-meta component :aligned keyword))

(defattr name (name)
  (add-meta component :name name))

;; 
;; Helpers
;; 

(defun get-name (component)
  (get-meta component :name))

(defun get-aligned (component)
  (get-meta component :aligned))

;; 
;; Children
;;  

(defclass children (attribute)
  ((children :type 'list
	     :initarg :children)
   (priority :allocation :class
	     :initform 4)))

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

(defvar west
  (jfield "java.awt.BorderLayout" "WEST"))

(defvar east
  (jfield "java.awt.BorderLayout" "EAST"))

(defvar north
  (jfield "java.awt.BorderLayout" "NORTH"))

(defvar south
  (jfield "java.awt.BorderLayout" "SOUTH"))

(defun resolve-alignment (align)
  (ecase align
    (:west west)
    (:east east)
    (:north north)
    (:south south)))


(defclass layout (attribute)
  ((layout :type :keyword
	   :initarg :layout)
   (axis :initarg :axis)))


(defun box-layout (c axis)
  (jnew "javax.swing.BoxLayout" c axis))


(defun flow-layout ()
  (jnew "java.awt.FlowLayout"))


(defun border-layout ()
  (jnew "java.awt.BorderLayout"))


(defun make-layout (a c)
  (ecase (slot-value a 'layout)
    (:box (box-layout c (slot-value a 'axis)))
    (:flow (flow-layout))
    (:border (border-layout))))


(defmethod apply-attribute ((a layout) c)
  (let ((c (wrapped-java-object c)))
    (set-layout c (make-layout a c))))

(defmethod apply-attribute ((a layout) (c frame))
  (let ((pane (java-call "getContentPane" c)))
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
