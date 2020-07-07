;; Import plotting library
(ql:quickload :vgplot) 

;;Higher float precision
(setq *read-default-float-format* 'double-float)

;; Define Variables for later use
(defvar y (list))
(defvar o1 (list))
(defvar o2 (list))
(defvar s (list))

;; For some reason this is not implemented in common Lisp
(defun factorial (n)
    (if (= n 1)              
		  1                           
		  (* n (factorial (- n 1))))) 

;; Recursion function for H_n
(defun calc_h (x n) 
	(when (= n 1)
	   ;; Base Case
	   (setq y (list (* 2 x) 1))
	)(unless (= n 1)
		;; Get previous values
		(setq y (calc_h x (- n 1)))

		;; Calculate new value
		(setq y (cons (* 2 (- (* x (car y)) (* (- n 1) (car (cdr y))))) y))
	)
	(append y)
)

;; Compute Quantum theory- and classical probabilities
(defun h_range (n &key (min -15.0d0) (max 15.0d0) (step 0.1d0)) 
  (loop for x from min below max by step
		do 
		   ;; Quantum model
			(setq o1 (cons 
				(* 
				(expt (/ 1.0d0 (sqrt (* (sqrt pi) (expt 2 n) (factorial n)))) 2)
				(expt (car (calc_h x n)) 2)
				(exp (- (expt x 2)))
				)
			o1))

			;; Classical Model
			(setq o2 (cons
				(realpart (/ 1.0d0
				(* pi (sqrt (- (+ (* 2 n) 1.1d0) (expt x 2))))
				))
			o2))

			;; Range for X-axis
			(setq s (cons x s))
		)
  (list o1 o2 s)
  )

;; Plot the result
(defvar res (h_range 100))
(vgplot:plot (caddr res) (car res) "b;|ψ_n(x)|^2" 
				 (caddr res) (cadr res) "r;ω_n(x)")

