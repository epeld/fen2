

;; 
;; Component Framework
;; 

(defclass component ()
  ((instance :documentation "The java wrapped java instance"
	     :initarg :instance
	     :reader wrapped-java-object)
   (children :documentation "All the subcomponents of this component"
	     :initarg :children
	     :initform nil
	     :accessor subcomponents)))

(defmacro defcomponent (name qualified-import &optional (component-superclass 'component))
  (let ((var (gensym))
	(attrs (gensym)))
    
    `(progn

       ;; To allow for dispatching on this specific component:
       (defclass ,name (,component-superclass)
	 ())
       
       (defun ,name (&rest ,attrs)
	 (let ((,var (make-instance ',name 
				    :instance (jnew ,qualified-import))))
	   
	   (apply-attributes ,var ,attrs)

	   ,var)))))


(defun add-child (component child)
  "Add a child (sub) component"
  (jcall "add" (wrapped-java-object component) (wrapped-java-object child))
  (setf (subcomponents component) 
	(cons child (subcomponents component))))


(defun java-call (name component &rest args)
  (apply #'jcall name (slot-value component 'instance) args))

;; 
;; Components
;; 

(defcomponent frame "javax.swing.JFrame")
(defcomponent label "javax.swing.JLabel")
(defcomponent button "javax.swing.JButton")
(defcomponent panel "javax.swing.JPanel")

;; 
;; Setter utils
;; 

(defmacro defunj (name args &body body)
  "Shorthand for defining a java call on a component"
  (assert (consp args))
  `(defun ,name ,args
     (let ((,(car args) (wrapped-java-object (the component ,(car args)))))
       ,@body)))

(defunj set-visible (component visible)
  (jcall "setVisible" component visible))

(defunj set-title (component title)
  (jcall "setTitle" component title))

(defunj set-text (component text)
  (jcall "setText" component text))

(defunj set-size (component width height)
  (jcall "setSize" component width height))

(defunj set-opaque (component val)
  (jcall "setOpaque" component val))

(defunj set-foreground-color (component color)
  (jcall "setForeground" component color))

(defunj set-background-color (component color)
  (jcall "setBackground" component color))


;; 
;; Utils 
;; 

(defun string-color (name)
  (jfield "java.awt.Color" name))

(defunj pack (component)
  (jcall "pack" component))

(defun show (component)
  (pack component)
  (set-visible component t))


;; 
;; Example
;; 

(show 

 (frame 

  (title "Bae bakar!")
  (size 600 400)
  
  (layout :flow)
    
  (children
   
   (button (text "Tryck pa knappen!")
	   (color "green")
	   (background-color "blue"))
     
   (label (text "Vaelj mat")
	  (color "cyan"))
     
   (label (text "Hello, World!")
	  (background-color "pink")
	  (opaque t))
   
   (label (text "How are you today?")
	  (color "red")))))
