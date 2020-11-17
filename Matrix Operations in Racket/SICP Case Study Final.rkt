#lang racket

;Make Shape Object
;Input: rows - Integer, cols - Integer
;Output: '(rows . cols)
(define (make-shape rows cols)
  (cons rows cols))

;Get Rows from shape object
;Input: '(rows . cols)
;Output: rows - Integer
(define (get-rows shape)
  (car shape))

;Get Cols from shape object
;Input: '(rows . cols)
;Output: cols - Integer
(define (get-cols shape)
  (cdr shape))

;Make Matrix Object
;Input: '(rows . cols), lst - List
;Output: nested list
;        first element- shape of the matrix
;        second element- elements of the matrix
(define (make-matrix shape lst)
     (append (cons shape '())
             (append lst '())))

;Get elements of a matrix
(define (get-ele matrix)
  (cdr matrix))

;Get ith row jth element of a matrix
(define (get matrix i j)
  (list-ref (list-ref (get-ele matrix) i) j))

;Get shape of a matrix
(define (get-shape matrix)
  (car matrix))

;Diagonal Elements of a matrix
;Applicable only for square matrices
(define (d_helper m size)
  (cond
    [(empty? m) (list)]
    [(= size -1) (list)]
    [else (append (list (list-ref (list-ref m (+ size 1)) size)) (d_helper m (- size 1)))])) 
(define (diagonal matrix)
  (cond
    [(= (get-rows (get-shape matrix)) (get-cols (get-shape matrix))) (d_helper matrix (- (get-cols (get-shape matrix)) 1))]
    [else (error "Not a Square Matrix")]))

;Lower Triangle of a Square Matrix
(define (ele-j A i j n)
  (cond
    [(= j n) (list)]
    [(> j i) (append (list 0) (ele-j A i (+ j 1) n))]
    [else (append (list (list-ref (list-ref A i) j)) (ele-j A i (+ j 1) n))]))

(define (ele-i A i n)
  (cond
    [(= i n) (list)]
    [else (append (list (ele-j A i 0 n)) (ele-i A (+ i 1) n))]))

(define (lower-tri matrix)
  (cond
    [(= (get-rows (get-shape matrix)) (get-cols (get-shape matrix))) (append (list (get-shape matrix)) (ele-i (get-ele matrix) 0 (get-rows (get-shape matrix))))]
    [else (error "Square Matrix Expected")]))

;Upper Triangle of a Square Matrix
(define (ele-jj A i j n)
  (cond
    [(= j n) (list)]
    [(< j i) (append (list 0) (ele-jj A i (+ j 1) n))]
    [else (append (list (list-ref (list-ref A i) j)) (ele-jj A i (+ j 1) n))]))

(define (ele-ii A i n)
  (cond
    [(= i n) (list)]
    [else (append (list (ele-jj A i 0 n)) (ele-ii A (+ i 1) n))]))

(define (upper-tri matrix)
  (cond
    [(= (get-rows (get-shape matrix)) (get-cols (get-shape matrix))) (append (list (get-shape matrix)) (ele-ii (get-ele matrix) 0 (get-rows (get-shape matrix))))]
    [else (error "Square Matrix Expected")]))

;Matrix A
(define shapeA (make-shape 3 3))
(define A (make-matrix shapeA '((1 2 3) (4 5 6) (7 8 9))))

;Matrix B
(define shapeB (make-shape 4 4))
(define B (make-matrix shapeB '((1 2 3 4) (1 2 3 4) (5 6 7 8) (5 6 7 9))))
;Diagonal of B
(define B_D (diagonal B))

;Not a square matrix example C
(define shapeC (make-shape 2 3))
(define C (make-matrix shapeC '((1 2 3) (4 5 6))))

;Performs operation on two lists
;Helps Add, Subtract
(define (op-lists op a b)
  (map op a b))

;Iterates two matrices row by row and applies 'op' for the rows 
;Helps Add, Subtract
(define (helper op A B size)
  (cond
    [(= size 0) '()]
    [else (append (list (op-lists op (car A) (car B))) (helper op (cdr A) (cdr B) (- size 1)))]))

;Addition/Subtraction of two matrices of shape n x m
(define (operation op x y)
  (cond
    [(and (= (get-rows (get-shape x)) (get-rows (get-shape y))) (= (get-cols (get-shape x)) (get-cols (get-shape y))))
     (append (list (get-shape x)) (helper op (get-ele x) (get-ele y) (get-rows (get-shape x))))]
    [else (error "Shape Error")]))

;Addition main
(define (add x y)
  (operation + x y))

;Subtraction main
(define (sub x y)
  (operation - x y))

;Scalar Multiplication
(define (multiplier lst val)
  (cond
    [(empty? lst) (list)]
    [else (append (list (* (car lst) val)) (multiplier (cdr lst) val))]))

(define (help A val)
  (cond
    [(empty? A) (list)]
    [else (append (list (multiplier (car A) val)) (help (cdr A)  val))]))

(define (s-multi matrix val)
  (append (list (get-shape matrix)) (help (get-ele matrix) val)))

;Multiplication of two matrices
(define mult_mat
  (λ (A B)
    (Trans_Mat (map (λ (x) (mul_Mat_vec A x))
                    (Trans_Mat B)))))
(define Trans_Mat
  (λ (A)
    (apply map (cons list A))))

(define mul_Mat_vec
  (λ (A v)
    (map (λ (x) (apply + (map * x v)))
         A)))
(define (multiply matrix1 matrix2)
  (cond
    [(= (get-cols (get-shape matrix1)) (get-rows (get-shape matrix2))) (append (list (make-shape (get-rows (get-shape matrix1)) (get-cols (get-shape matrix2)))) (mult_mat (get-ele matrix1) (get-ele matrix2)))]
    [else (error "Multiplication Shape Error")]))


  

;Example Matrix 1
(define shape1 (make-shape 2 2))
(define matrix1 (make-matrix shape1 '((1 2) (3 4))))

;Example Matrix 2
(define shape2 (make-shape 2 2))
(define matrix2 (make-matrix shape2 '((2 4) (8 9))))

;Addition of Matrix 1 and Matrix 2
(define sum12 (add matrix1 matrix2))
;Subtraction of Matrix 1 and Matrix 2
(define sub12 (sub matrix1 matrix2))


;Transpose of a matrix
(define trans
  (lambda (xss)
    (cond
      [(empty? xss)         empty]
      [(empty? (first xss)) empty]
      [else                 (define first-column   (map first xss))
                            (define other-columns  (map rest  xss))
                            (cons first-column
                                  (trans other-columns))])))
(define (transpose matrix)
  (append (list (get-shape matrix)) (trans (get-ele matrix))))
