(defun my-assert (a b)
    (if (equal a b)
        (print (format nil "Correct. Expected: ~a Got: ~a" b a))
        (print (format nil "Incorrect! Expected: ~a Got: ~a" b a))))

; Problem 1
(defun count-all (target lst)
    (cond
        ((equal target lst) 1)
        ((or (not (listp lst)) (null lst)) 0)
        (t (+ (count-all target (car lst)) (count-all target (cdr lst))))))

(print "TEST: count-all")
(my-assert (count-all 'a '((a b) c () ((d (e))))) 1)
(my-assert (count-all 'a '(b (a n) a n a)) 3)
(my-assert (count-all 'a '((a b (c a))(b (a c) a))) 4)
(my-assert (count-all '(c a) '((a b (c a))(b (c a) a))) 2)
(terpri)

; Problem 2
(defun remove-all2 (target lst)
    (cond
        ((atom lst) lst)
        ((equal target (car lst)) (remove-all2 target (cdr lst)))
        (t (cons (remove-all2 target (car lst)) (remove-all2 target (cdr lst))))))
        
(print "TEST: remove-all2")
(my-assert (remove-all2 'a '(b (a n) a n a)) '(b (n) n))
(my-assert (remove-all2 'a '((a b (c a))(b (a c) a))) '((b (c))(b (c))))
(my-assert (remove-all2 '(a b) '(a (a b)((c (a b)) b))) '(a ((c) b)))
(terpri)

; Problem 3
(defun reverse-all (lst)
    (cond
        ((consp lst) (append (reverse-all (cdr lst)) (list (reverse-all (car lst)))))
        (t lst)))

(print "TEST: reverse-all")
(my-assert (reverse-all '((1 2) (3 4) 5)) '(5 (4 3) (2 1)))
(my-assert (reverse-all '(a (b c)(d (e f)))) '(((f e) d)(c b) a))
(terpri)

; Problem 4
(defun depth (lst)
    (cond
        ((atom lst) 0)
        (t (+ 1 (reduce #'max (mapcar #'depth lst))))))
        
(print "TEST: depth")
(my-assert (depth ()) 0)
(my-assert (depth '(a b c)) 1)
(my-assert (depth '(a (b c))) 2)
(my-assert (depth '((a b) c (d ((f))))) 4)
(terpri)

; Problem 5
(defun flatten (lst)
    (cond
        ((null lst) nil)
        ((atom (car lst)) (cons (car lst) (flatten (cdr lst))))
        (t (append (flatten (car lst)) (flatten (cdr lst))))))

(print "TEST: flatten")
(my-assert (flatten '((a b) c (d ((f))))) '(a b c d f))
(terpri)

; Problem 6
(defun remove-left-most (target lst)
    (cond
        ((atom lst) lst)
        ((equal target (car lst)) (cdr lst))
        ((atom (car lst)) (cons (car lst) (remove-left-most target (cdr lst))))
        (t (let ((temp (remove-left-most target (car lst))))
                (cond
                    ((equal temp (car lst)) (cons temp (remove-left-most target (cdr lst))))
                    (t (cons temp (cdr lst))))))))
        
(print "TEST: remove-left-most")
(my-assert (remove-left-most 'b '(a (b c)(c (b a)))) '(a (c)(c (b a))))
(my-assert (remove-left-most '(c d) '((a (b c))((c d) e))) '((a (b c))(e)))
(terpri)

; Problem 7
(defun find-subst-helper (str lst)
    (cond
        ((equal (car str) (car lst))
            (cond
                ((equal (length str) 1) t)
                (t (find-subst-helper (cdr str) (cdr lst)))))
        (t nil)))

(defun find-subst (str lst)
    (cond
        ((> (length str) (length lst)) nil)
        ((find-subst-helper str lst) lst)
        (t (find-subst str (cdr lst)))))

(print "TEST: find-subst")
(my-assert (find-subst '(b a b o) '(b b a a b b a b o o b b)) '(b a b o o b b))
(terpri)

; Problem 8
(defun subst-count (str lst)
    (cond
        ((> (length str) (length lst)) 0)
        ((find-subst-helper str lst) (+ 1 (subst-count str (cdr lst))))
        (t (subst-count str (cdr lst)))))

(print "TEST: subst-count")
(my-assert (subst-count '(b a b) '(b b a b a b b a a b)) 2)
(terpri)

; Problem 9
(defun select-helper (each test)
    (if (apply test (cdr each))
        (car each)
        nil))

(defun select (db test)
    (cond
        ((equal (length db) 0) nil)
        ((select-helper (car db) test) (cons (caar db) (select (cdr db) test)))
        (t (select (cdr db) test))))

(print "TEST: select")
(my-assert (select '((tom 22)(jack 25)(joe 44)(judy 10)) #'(lambda (n)(> n 23))) '(jack joe))
(terpri)

; Problem 10-a
(defun car-string (n l)
    (cond
        ((equal n 0) nil)
        (t (cons (car l) (car-string (- n 1) (cdr l))))))

(print "TEST: car-string")
(my-assert (car-string 3 '(a b c d e)) '(a b c))
(my-assert (car-string 4 '(a b c d e)) '(a b c d))
(terpri)

; Problem 10-b
(defun equal-except (n l1 l2)
    (cond
        ((= n -1) nil)
        ((= n (length l1)) t)
        ((equal (car l1) (car l2)) (equal-except n (cdr l1) (cdr l2)))
        (t (equal-except (- n 1) (cdr l1) (cdr l2)))))

(print "TEST: equal-except")
(my-assert (equal-except 2 '(a b c d) '(b b c c)) t)
(my-assert (equal-except 3 '(a b c d e) '(e f g a b)) nil)
(terpri)

(defun subst-count (str lst)
    (cond
        ((> (length str) (length lst)) 0)
        ((find-subst-helper str lst) (+ 1 (subst-count str (cdr lst))))
        (t (subst-count str (cdr lst)))))

; Problem 10-c
(defun find-sim-st (n l1 l2)
    (cond
        ((> (length l1) (length l2)) 0)
        ((equal-except n l1 l2) (+ 1 (find-sim-st n l1 (cdr l2))))
        (t (find-sim-st n l1 (cdr l2)))))

(print "TEST: find-sim-st")
(my-assert (find-sim-st 1 '(b a b) '(b b a b a b b a a b)) 4)
(my-assert (find-sim-st 2 '(b a b) '(b b a b a b b a a b)) 7)
(terpri)
; 10-c  (find-sim-st n l1 l2)  count similar substrings of l1 from l2. 
; (find-sim-st 1 '(b a b) '(b b a b a b b a a b)) ==> 4
; (find-sim-st 2 '(b a b) '(b b a b a b b a a b)) ==> 7