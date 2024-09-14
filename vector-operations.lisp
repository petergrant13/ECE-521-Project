; Author Peter Grant
; Started September 13, 2024
; Part of the project for ECE 521

; ------------------------------------------------------------------------------
; Use defvar instead of setf or something to initialize arrays
(defvar *my-array* (make-array 5) "A global array.")
(defvar *foo* #(69 420 666))

;; Now you can modify its elements
(setf (aref *my-array* 0) 25)
(setf (aref *my-array* 1) 24)
(setf (aref *my-array* 2) 45)
(setf (aref *my-array* 3) 10)
(setf (aref *my-array* 4) 20)

; can also use similar stuff in the REPL to have it behave more like matlab
;-------------------------------------------------------------------------------

; Vector Operations 



; the cross product will implement the Levi-Civita symbol for obvious reasons
(defun levi-civita (i j k)
  "Compute the Levi-Civita symbol ε_ijk for indices i, j, k.
   Returns 1, -1, or 0 depending on the permutation."
  (cond
    ;; Ensure indices are in the range [1, 2, 3]
    ((or (not (member i '(1 2 3)))
         (not (member j '(1 2 3)))
         (not (member k '(1 2 3))))
     (error "Indices must be 1, 2, or 3."))
    ;; Check for duplicate indices
    ((or (= i j) (= j k) (= i k)) 0)
    ;; Check the permutations for Levi-Civita symbol
    ((and (= i 1) (= j 2) (= k 3)) 1)
    ((and (= i 1) (= j 3) (= k 2)) -1)
    ((and (= i 2) (= j 1) (= k 3)) -1)
    ((and (= i 2) (= j 3) (= k 1)) 1)
    ((and (= i 3) (= j 1) (= k 2)) 1)
    ((and (= i 3) (= j 2) (= k 1)) -1)
    ;; Default case (although we shouldn't reach this)
    (t 0)))

; now compute the cross-product
(defun cross-product (vec1 vec2)
  "Compute the cross product of two 3D vectors VEC1 and VEC2.
   Both vectors must be of type VECTOR and of length 3."
  (if (or (not (vectorp vec1)) (not (vectorp vec2))
          (/= (length vec1) 3) (/= (length vec2) 3))
      (error "Both vectors must be vectors of length 3."))
    (let* ((v1-x (aref vec1 0))
           (v1-y (aref vec1 1))
           (v1-z (aref vec1 2))
           (v2-x (aref vec2 0))
           (v2-y (aref vec2 1))
           (v2-z (aref vec2 2)))
      (vector (- (* v1-y v2-z) (* v1-z v2-y))
              (- (* v1-z v2-x) (* v1-x v2-z))
              (- (* v1-x v2-y) (* v1-y v2-x)))))






