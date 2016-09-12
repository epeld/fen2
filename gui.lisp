

(defvar *window*)


(defvar *box-axis* 
  (jfield "javax.swing.BoxLayout" "LINE_AXIS"))

(defvar *application-modal* 
  (jfield "java.awt.Dialog$ModalityType" "APPLICATION_MODAL"))



(defun alert (message &optional (frame *window*))
  (jstatic "showMessageDialog" "javax.swing.JOptionPane" frame message))


(defun action-listener (fun)
  (jinterface-implementation 
   "java.awt.event.ActionListener"
   
   "actionPerformed"
   (lambda (action)
     (funcall fun action))))


(defun make-button (title &optional on-click)
  (let ((b (jnew "javax.swing.JButton" title)))
    (when on-click
      (jcall "addActionListener" b (action-listener on-click)))
    b))


(defun make-box-layout (pane &optional (axis *box-axis*))
  (jnew "javax.swing.BoxLayout" pane axis))



(defun set-visible (window visible)
  (jcall "setVisible" window visible))


(defun open-window (&optional (title "Recipes"))
  (let ((wnd (jnew "javax.swing.JFrame" title))
	(pane (jnew "javax.swing.JPanel")))
    (make-box-layout pane)
    (jcall "add" wnd pane)
    (jcall "add" pane (make-button "Generate Suggestion" (lambda (x) (alert "You clicked 'generate'!"))))
    (jcall "add" pane (make-button "Add new Recipe" (lambda (x) (alert "You clicked 'Add'!"))))
    (jcall "add" pane (make-button "List Recipes" (lambda (x) (alert "You clicked 'List'"))))
    (jcall "pack" wnd)
    (set-visible wnd t)
    wnd))


(defun open-dialog (&optional (owner *window*))
  (let ((dlg (jnew "javax.swing.JDialog" owner "Hello?" t)))
    (set-visible dlg t)
    (jcall "add" dlg (jnew "javax.swing.JLabel" "Test"))
    (jcall "add" dlg (jnew "javax.swing.JLabel" "Test2"))
    (jcall "pack" dlg)
    dlg))


'(:dialog
  (:title "My Dialog")
  (:layout :box)
  (:children
   (:label "Hello World?")
   (:label "How are you?")))


(setf *window* (open-window))

(open-dialog)
