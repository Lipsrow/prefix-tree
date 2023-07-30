;Префиксное дерево
;основные функции:
;1.get_tree - получить дерево из списка слов
;2.add_word - добавить новое слово в дерево
;3.check_word - проверить наличие слова в дереве
;4.del_word - удалить слово из дерева
;5.print_tree - печатает дерево 

(defun get_num(L A N) 
(cond ((eq L (car A)) N)
      (T (get_num L (cdr A) (+ 1 N)))))
      
(defun let_num (L) ;получить номер буквы по алфавиту
(get_num L '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z) 1)) 


(defun add_new_word(L) ; (L K N) -> ((L () (K () (N *))))
(cond ((null L) '*)
      ((null (cdr L)) (cons (car L) (cons (add_new_word (cdr L)) NIL)))
	  (T (cons (car L) (cons NIL (cons (add_new_word (cdr L)) NIL))))))
	  
	  
(defun get_tree(L) ;сделать дерево из списка 
(cond ((null L) NIL)
      (T (add_word (car L) (get_tree (cdr L))))))
      
      
(defun add_word (L W) ;добавить новое слово
(cond ((and (null W) (null L)) NIL)
      ((null W) (cons (add_new_word L) W))
      ((and (eq (caar W) (car L)) (null (cdr L)) (cons (cons (caar W) (cons '* (add_word (cdr L) (cddar W)))) (cdr W))))
      ((eq (caar W) (car L)) (cons (cons (caar W) (cons (cadar W) (add_word (cdr L) (cddar W)))) (cdr W)))
      ((< (let_num (car L)) (let_num (caar W))) (cons (add_new_word L) W))
      (T (cons (car W) (add_word L (cdr W))))))
	  
	  
(defun check_word(L W) ;проверить наличие слова
(cond ((and (not (null L)) (null W)) NIL)
      ((and (eq (caar W) (car L)) (null (cdr L))) (cond ((eq (cadar W) '*) T)
                                     (T NIL)))
      ((eq (caar W) (car L)) (check_word (cdr L) (cddar W)))
      (T (check_word L (cdr W)))))

(defun del_word1 (L W) ;меняет * на NIL при удалении слова
(cond ((or (null L) (null W)) W)
      ((and (eq (caar W) (car L)) (null (cdr L)) (cons (cons (caar W) (cons NIL (del_word1 (cdr L) (cddar W)))) (cdr W))))
      ((eq (caar W) (car L)) (cons (cons (caar W) (cons (cadar W) (del_word1 (cdr L) (cddar W)))) (cdr W)))
      (T (cons (car W) (del_word1 L (cdr W))))))
      
      
(defun list2del (L) ;имеет ли список вид (A NIL)
(cond ((and (symbolp (car L)) (eq (cadr L) NIL) (null (cddr L))) T)
      (T NIL)))


(defun has2del (W) ;есть ли в списке список, который надо удалить
(cond ((null W) NIL)
      ((symbolp (car W)) (has2del (cdr W)))
      ((list2del (car W)) T)
      (T (or (has2del (car W)) (has2del (cdr W))))))


(defun delpair(W) ;удаляет одну пару вида (A NIL)
(cond ((null W) NIL)
      ((symbolp (car W)) (cons (car W) (delpair (cdr W))))
      ((list2del (car W)) (delpair (cdr W)))
      (T (cons (delpair (car W)) (delpair (cdr W))))))


(defun del_all_pairs(W) ;удаляет все ненужные списки
(cond ((has2del W) (del_all_pairs (delpair W)))
      (T W)))
     
(defun del_words (L W)
(cond ((null L) W)
      (T (del_words (cdr L) (del_word (car L) W)))))
 
(defun del_word (L W) ;удаляет слово
(cond ((check_word L W) (del_all_pairs (del_word1 L W)))
      (T W)))
      
      

(defun get_words (W N) (get_words1 W N NIL NIL)) ;получить слова для списка

(defun get_words1 (W N Now Res)
(cond ((null W) Res)
      ((null (car W)) (get_words1 (cdr W) N Now Res))
      ((listp (car W)) (get_words1 (cdr W) N Now (get_words1 (car W) N Now Res)))
      ((eq (car W) '*) (cond ((> (length Now) N) (get_words1 (cdr W) N Now Res))
                              (T (get_words1 (cdr W) N Now (add_end Now Res)))))
      ((atom (car W)) (get_words1 (cdr W) N (append Now (cons (car W) NIL)) Res))))

        
(defun add_end (X L) ;добавить элемент Х в конец списка L
(cond ((null L) (cons X L))
      (T (cons (car L) (add_end X (cdr L))))))


(defun replace2print (X Y) ;заменяет буквы одного слова на звездочки, основываясь на слово перед ним
(cond ((null Y) X)
      ((eq (car X) (car Y)) (cons '* (replace2print (cdr X) (cdr Y))))
      (T X)))

(defun replace_all (X) (reverse (replace_all1 (reverse X)))) ;заменяет все слова в списке слов на звездочки для вывода

(defun replace_all1 (X) 
(cond ((or (null X) (null (cdr X))) X)
      (T (cons (replace2print (car X) (cadr X)) (replace_all1 (cdr X))))))
     
(defun print_list (X) (format T "~{~a ~} ~%" X)) ;печатает список без скобок

(defun print_all_words (X) (format T "~%") (print_all_words1 X)) ;печатает все слова в списке в столбик

(defun print_all_words1 (X)
(cond ((null X))
      (T (print_list (car X)) (print_all_words1 (cdr X)))))
      
      
(defun print_tree(W N)(print_all_words (replace_all (get_words W N)))) ;печатает дерево

;(print (get_tree '((A B) (A C) (A B C) (T E A) (T E S T))))
;(print_tree (get_tree '((A B) (A C) (A B C)(T E A) (T E S T))) 5)
;(print_tree (get_tree '((A B) (A C) (A B C)(T E A) (T E S T))) 3)
;(print (check_word '(T E S T) (get_tree '((T E A) (T E S T)))))
;(print (check_word '(T E S T) (del_word '(T E S T) (get_tree '((T E A) (T E S T))))))
;(print_tree (add_word '(A B) (get_tree '((A B)))) 2)

(print_tree (get_tree '((C A T) (C A P) (C A P T I O N) (C A T T Y))) 7)
