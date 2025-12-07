;;; Функціональний підхід
(defun bubble-pass-functional (lst)
  (if (or (null lst) (null (cdr lst)))
      (values lst nil)
      (let ((x (car lst))
            (y (cadr lst)))
        (if (> x y)
            (multiple-value-bind (rest swapped)
                (bubble-pass-functional (cons x (cddr lst)))
              (values (cons y rest) t))
            (multiple-value-bind (rest swapped)
                (bubble-pass-functional (cdr lst))
              (values (cons x rest) swapped))))))

(defun functional-sort (lst)
  (multiple-value-bind (new-list swapped) 
      (bubble-pass-functional lst)
    (if swapped
        (functional-sort new-list)
        new-list)))              

;;; Імперативний підхід
(defun imperative-sort (lst)
  (let ((result (copy-list lst))
        (len (length lst))
        (flag t))
    
    (do () ((not flag) result)
      (setf flag nil) 
      
      (dotimes (i (- len 1))
        (let ((current (nth i result))
              (next (nth (1+ i) result)))
          (when (> current next)
            (rotatef (nth i result) (nth (1+ i) result))
            (setf flag t)))))
    result))

;;; Тести
(defun check-sort (name func input expected)
  "Execute `my-reverse' on `input', compare result with `expected' and print
    comparison status"
  (format t "~:[FAILED~;passed~] ~a~%"
          (equal (funcall func input) expected)
          name))

(defun test-sorting ()
  (format t "~%--- Testing Functional Sort ---~%")
  (check-sort "test 1 (mixed)" #'functional-sort '(3 1 4 1 5 9 2) '(1 1 2 3 4 5 9))
  (check-sort "test 2 (sorted)" #'functional-sort '(1 2 3 4 5) '(1 2 3 4 5))
  (check-sort "test 3 (reverse)" #'functional-sort '(5 4 3 2 1) '(1 2 3 4 5))
  (check-sort "test 4 (empty)" #'functional-sort nil nil)
  
  (format t "~%--- Testing Imperative Sort ---~%")
  (check-sort "test 1 (mixed)" #'imperative-sort '(3 1 4 1 5 9 2) '(1 1 2 3 4 5 9))
  (check-sort "test 2 (sorted)" #'imperative-sort '(1 2 3 4 5) '(1 2 3 4 5))
  (check-sort "test 3 (reverse)" #'imperative-sort '(5 4 3 2 1) '(1 2 3 4 5))
  (check-sort "test 4 (empty)" #'imperative-sort nil nil))
