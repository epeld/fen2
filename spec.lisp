

(defun set-visible (window visible)
  (jcall "setVisible" window visible))


(defun frame (&rest attributes)
  (let ((f (jnew "javax.swing.JFrame")))
    (dolist (attr attributes)
      (apply-attribute attr f))
    f))


(defun label (&rest attributes)
  (let ((l (jnew "javax.swing.JLabel")))
    (dolist (attr attributes)
      (apply-attribute attr l))
    l))


(defun pack (component)
  (jcall "pack" component))


(defun show (component)
  (pack component)
  (set-visible component t))


(show 

 (frame 

  (title "Hello, World!")
  (size 500 300)
  (layout :flow)
  
  (children
   (label 
    (text "Hello, World!")
    (color "blue")
    (background-color "red"))
   (label (text "How are you today?")))))
