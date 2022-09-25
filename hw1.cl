(defun fib (n)
    (cond ((eql n 0) 0)
        ((eql n 1) 1)
        (t (+ (fib (- n 1)) (fib (- n 2))))))

(defun my-reverse (i)
    (cond ((eql i nil)())
        ((equal i (car i))(list i))
        (t (append (my-reverse (cdr i)) (list (car i))))))

(defun sum (n)
    (if (eql n 0)
        0
        (+ n (sum (- n 1)))))

(defun last-item (n)
    (if (eql (cdr n) nil)
        (car n)
        (last-item (cdr n))))

(defun remove-lst (word lst)
    (cond ((eql (car lst) nil) nil)
        ((equal word (car lst)) (cdr lst))
        (t (append (list (car lst)) (remove-lst word (cdr lst))))))

(defun remove-2nd (word lst)
    (remove-2nd-helper word lst 0))

(defun remove-2nd-helper (word lst count)
    (cond ((eql (car lst) nil) nil)
        ((and (equal word (car lst))
             (eql count 1))
         (cdr lst))
        ((equal word (car lst))
         (append (list (car lst)) (remove-2nd-helper word (cdr lst) 1)))
        (t (append (list (car lst)) (remove-2nd-helper word (cdr lst) count)))))

(defun remove-last (word lst)
    (my-reverse (remove-lst word (my-reverse lst))))

(defun remove-all1 (word lst)
    (cond ((eql (car lst) nil) nil)
        ((equal word (car lst)) (remove-all1 word (cdr lst)))
        (t (append (list (car lst)) (remove-all1 word (cdr lst))))))

(defun subst-1st (a b lst)
    (cond ((eql (car lst) nil) nil)
        ((equal b (car lst)) (append (list a) (cdr lst)))
        (t (append (list (car lst)) (subst-1st a b (cdr lst))))))

(defun subst-all (a b lst)
    (cond ((eql (car lst) nil) nil)
        ((equal b (car lst)) (append (list a) (subst-all a b (cdr lst))))
        (t (append (list (car lst)) (subst-all a b (cdr lst))))))

(define reflect (slst lst)
    (cond ((eql (car lst) nil) nil)
        (t (append (reflect-atom (car lst) slst) (reflect slst (cdr lst))))))

(define reflect-atom (a table)
    (cond ((eql (car lst) nil) nil)
        ((equal a (caar table)) (cadr table))
        (t (reflect-atom a (cdr table)))))