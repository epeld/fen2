(reify
 (frame 

  (title "Super Recipe-o-matic 3000!")
  
  (layout :box)
    
  (children
   
   (panel
    (layout :flow)
    (children (button (text "Browse Recipes")
		      (name "browse")
		      (action (lambda (x)
				(declare (ignore x))
				(alert (parent) (format nil "Parent is ~a and root is ~a" (parent) (parent -1))))))
		
	      (button (text "Randomize!")
		      (name "randomize"))))
   
   (panel
    (preferred-size 300 300)
      
    (layout :flow)
    (children

     (label (text "Hello, World!")
	    (background-color "pink")
	    (opaque t))
   
     (label (text "How are you today?")
	    (color "red")))))))
