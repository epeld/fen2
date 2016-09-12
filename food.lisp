
;; TODO create some UI logic in terms of generic components, e.g 'button' 'area' etc
;; then make a concrete implementation using java swing in ABCL

;; 
;; Recipes
;; 

(defvar *recipes*
  "List of all recipes known to the system")


(defun make-recipe (name description &optional ingredients)
  "Construct a new recipe object"
  (list :name name
	:description description
	:ingredients ingredients))


(defun add-recipe (recipe)
  "Add a new recipe to the database"
  (push recipe *recipes*))


;; 
;; List utils
;; 

(defun any-item (list)
  "Choose an item at random from the list"
  (nth (random (length list))
       list))


(defun choose-randomly (items &optional (count 1))
  "Select a number of items at random"
  (if (or (zerop count) (endp items))
      nil
      (let ((item (any-item items)))
	(cons item (choose-randomly (remove item items) (- count 1))))))

