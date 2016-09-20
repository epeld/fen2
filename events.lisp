
(defun alert (parent message)
  "Show a JS-like alert with a message, given a parent frame"
  (the component parent)
  (jstatic "showMessageDialog" "javax.swing.JOptionPane" (wrapped-java-object parent) message))


(defun action-listener (fun)
  "Implements a java Action Listener, given a lambda to call"
  (jinterface-implementation 
   "java.awt.event.ActionListener"
   
   "actionPerformed"
   (lambda (action)
     (funcall fun action))))


(defattr action (fun)
  (jcall "addActionListener" (wrapped-java-object component) (action-listener fun)))
