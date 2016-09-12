

;; 
;; Component Framework
;; 

(defclass component ()
  ((instance :documentation "The java wrapped java instance"
	     :initarg :instance
	     :reader wrapped-java-object)))

(defmacro defcomponent (name qualified-import &optional (component-superclass 'component))
  (let ((var (gensym))
	(attrs (gensym))
	(attr (gensym)))
    
    `(progn

       ;; To allow for dispatching on this specific component:
       (defclass ,name (,component-superclass)
	 ())
       
       (defun ,name (&rest ,attrs)
	 (let ((,var (make-instance ',name 
				    :instance (jnew ,qualified-import))))
	   (dolist (,attr ,attrs)
	     (apply-attribute ,attr ,var))

	   ,var)))))

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

(defun set-visible (component visible)
  ;(jcall "setVisible" window visible)
  (java-call "setVisible" component visible))

(defun set-title (component title)
  (java-call "setTitle" component title))

(defun set-text (component text)
  (java-call "setText" component text))

(defun set-size (component width height)
  (java-call "setSize" component width height))

(defun set-opaque (component val)
  (java-call "setOpaque" component val))

(defun set-foreground-color (component color)
  (java-call "setForeground" component color))

(defun set-background-color (component color)
  (java-call "setBackground" component color))


;; 
;; Utils 
;; 

(defun string-color (name)
  (jfield "java.awt.Color" name))

(defun pack (component)
  (java-call "pack" component))

(defun show (component)
  (pack component)
  (set-visible component t))


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
