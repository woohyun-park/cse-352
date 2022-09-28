(defun my-assert (a b)
    (if (equal a b)
        (print (format nil "Correct. Expected: ~a Got: ~a" b a))
        (print (format nil "Incorrect! Expected: ~a Got: ~a" b a))))

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

(defun reverse-all (lst)
    (cond
        ((consp lst) (append (reverse-all (cdr lst)) (list (reverse-all (car lst)))))
        (t lst)))

(print "TEST: reverse-all")
(my-assert (reverse-all '((1 2) (3 4) 5)) '(5 (4 3) (2 1)))
(my-assert (reverse-all '(a (b c)(d (e f)))) '(((f e) d)(c b) a))
(terpri)

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

(defun flatten (lst)
    (cond
        ((null lst) nil)
        ((atom (car lst)) (cons (car lst) (flatten (cdr lst))))
        (t (append (flatten (car lst)) (flatten (cdr lst))))))

(print "TEST: flatten")
(my-assert (flatten '((a b) c (d ((f))))) '(a b c d f))
(terpri)

(defun remove-left-most (target lst)
    (print lst)
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