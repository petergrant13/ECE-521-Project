(defun matrix-multiply (A B)
  "Multiply two matrices A and B.
   Assumes A is an m x n matrix and B is an n x p matrix.
   Returns the result as an m x p matrix."
  (let* ((rows-a (length A))
         (cols-a (length (first A)))
         (cols-b (length (first B)))
         (result (make-array (list rows-a cols-b) :initial-element 0)))
    (loop for i from 0 to (1- rows-a)
          do (loop for j from 0 to (1- cols-b)
                   do (setf (aref result i j)
                            (reduce #'+
                                    (loop for k from 0 to (1- cols-a)
                                          collect (* (aref (aref A i) k)
                                                      (aref (aref B k) j)))))))
    result))



(defun matrix-add (A B)
  "Add two matrices A and B."
  (let ((rows (length A))
        (cols (length (first A))))
    (make-array (list rows cols)
                :initial-element 0
                :element-type 'number
                :initial-contents
                (loop for i from 0 below rows
                      collect (loop for j from 0 below cols
                                     collect (+ (aref (aref A i) j)
                                                (aref (aref B i) j)))))))


