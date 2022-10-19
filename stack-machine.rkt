#lang racket
(require "opcodes.rkt")
(provide make-stack-machine)
(provide run-stack-machine)
(provide get-stack)
(provide get-varnames)
(provide get-consts)
(provide get-names)
(provide get-code)
(provide get-IC)
(provide empty-stack)
(provide make-stack)
(provide push)
(provide pop)
(provide top)


;; TODO 1:
;; Alegeți metoda de reprezentarea a unei stive.
;; Implementați:
(define empty-stack '() )
(define (make-stack) empty-stack)

(define (push element stack) (append (list element) stack))
(define (top stack) (car stack))
(define (pop stack) (if (null? stack)
                        empty-stack
                        (cdr stack)))
;; TODO 2:
;; Alegeți metoda de reprezentare a unei mașini stivă.
;; Definiți make-stack-machine, acesta trebuie sa primeasca cele 4 segmente de date
;; Veți avea nevoie de o stivă pentru execuție și un counter ca să stiți
;; la ce instrucțiune sunteți.

(define (make-stack-machine stack co-varnames co-consts co-names co-code IC)
  (list stack co-varnames co-consts co-names co-code IC))
  
;; Definiți funcțiile `get-varnames`, `get-consts`, `get-names`,
;; `get-code`, `get-stack`, `get-IC` care primesc o mașina stivă și întorc
;; componenta respectivă

;; ex:
;; > (get-varnames (make-stack-machine empty-stack 'dummy-co-varnames (hash) (hash) (list) 0))
;; 'dummy-co-varnames

(define (get-varnames stack-machine) (car (cdr stack-machine)))
                                         

;; ex:
;; > (get-consts (make-stack-machine empty-stack (hash) 'dummy-co-consts (hash) (list) 0))
;; 'dummy-co-consts

(define (get-consts stack-machine) (car (cdr (cdr stack-machine))))

;; ex:
;; > (get-names (make-stack-machine empty-stack (hash) (hash) 'dummy-co-names (list) 0))
;; 'dummy-co-names

(define (get-names stack-machine) (car (cdr (cdr (cdr stack-machine)))))

;; ex:
;; > (get-code (make-stack-machine empty-stack (hash) (hash) (hash) 'dummy-co-code 0))
;; dummy-co-code

(define (get-code stack-machine) (car (cdr (cdr (cdr (cdr stack-machine))))))

;; Întoarce stiva de execuție.
;; ex:
;; > (get-code (make-stack-machine 'dummy-exec-stack (hash) (hash) (hash) (list) 0))
;; dummy-exec-stack

(define (get-stack stack-machine) (car stack-machine))

;; Întoarce instruction counterul.
;; ex:
;; > (get-code (make-stack-machine empty-stack (hash) (hash) (hash) (list) 0))
;; 0

(define (get-IC stack-machine) (car (cdr (cdr (cdr (cdr (cdr stack-machine)))))))



(define symbols (list 'STACK 'CO-VARNAMES 'CO-CONSTS 'CO-NAMES 'CO-CODE 'INSTRUCTION-COUNTER))

;; TODO 3:
;; Definiți funcția get-symbol-index care gasește index-ul simbolului in listă.
(define (my_get_index L element ind)
  (if (equal? (car L) element)
      ind
      (my_get_index (cdr L) element (add1 ind))))

(define (get-symbol-index symbol)
  (my_get_index symbols symbol 0))

;; Definiți funcția update-stack-machine care intoarce o noua mașina stivă
;; înlocuind componenta corespondentă simbolului cu item-ul dat în paremetri.
;; > (get-varnames (update-stack-machine "new-varnames" 'CO-VARNAMES stack-machine))
;; "new-varnames"
;; > (get-varnames (update-stack-machine "new-names" 'CO-NAMES stack-machine))
;; "new-names"
(define (update-stack-machine item symbol stack-machine)
  (list-set stack-machine (get-symbol-index symbol) item))

;; Definiți funcția push-exec-stack care primește o masină stivă și o valoare
;; și intoarce o noua mașina unde valoarea este pusă pe stiva de execuție
(define (push-exec-stack value stack-machine)
  (update-stack-machine (push value (get-stack stack-machine)) 'STACK stack-machine))

;;  Definiți funcția pop-exec-stack care primește o masină stivă
;;  și intoarce o noua mașina aplicând pop pe stiva de execuție.
(define (pop-exec-stack stack-machine)
  (if (null? (get-stack stack-machine))
      stack-machine
      (update-stack-machine (pop (get-stack stack-machine)) 'STACK stack-machine)))

;; TODO 4:
;; Definiți funcția run-stack-machine care execută operații pană epuizează co-code.

(define (incrementare_IC delta stack-machine)
  (update-stack-machine (+ delta (get-IC stack-machine)) 'INSTRUCTION-COUNTER stack-machine))
 
(define (POP_TOP stack-machine) (incrementare_IC 1 (pop-exec-stack stack-machine)))

(define (LOAD_CONST index stack-machine)
  (incrementare_IC 1 (push-exec-stack (hash-ref (get-consts stack-machine) index) stack-machine)))

(define (LOAD_FAST index stack-machine)
  (incrementare_IC 1 (push-exec-stack (hash-ref (get-varnames stack-machine) index) stack-machine)))

(define (LOAD_GLOBAL index stack-machine)
  (incrementare_IC 1 (push-exec-stack (hash-ref (get-names stack-machine) index) stack-machine)))

(define (STORE_FAST key stack-machine)
  (incrementare_IC 1 (pop-exec-stack
                      (update-stack-machine
                       (hash-set (get-varnames stack-machine) key (top (get-stack stack-machine)))
                       'CO-VARNAMES stack-machine)
                      )))

(define (BINARY_MODULO stack-machine)
  (incrementare_IC 1 (push-exec-stack
                      (modulo (top (cdr (get-stack stack-machine)))
                         (top (get-stack stack-machine)))
                      (pop-exec-stack (pop-exec-stack stack-machine)))))

(define (BINARY_ADD stack-machine)
  (incrementare_IC 1 (push-exec-stack
                      (+ (top (cdr (get-stack stack-machine)))
                         (top (get-stack stack-machine)))
                      (pop-exec-stack (pop-exec-stack stack-machine)))))

(define (BINARY_SUBTRACT stack-machine)
  (incrementare_IC 1 (push-exec-stack
                      (- (top (cdr (get-stack stack-machine)))
                         (top (get-stack stack-machine)))
                      (pop-exec-stack (pop-exec-stack stack-machine)))))

(define (INPLACE_ADD stack-machine) (BINARY_ADD stack-machine))
(define (INPLACE_SUBTRACT stack-machine) (BINARY_SUBTRACT stack-machine))
(define (INPLACE_MODULO stack-machine) (BINARY_MODULO stack-machine))

(define (JUMP_ABSOLUTE target stack-machine)
  (update-stack-machine (/ target 2) 'INSTRUCTION-COUNTER stack-machine))

(define (COMPARE_OP index stack-machine)
  (incrementare_IC 1 (push-exec-stack
                      ((get-cmpop index) (top (cdr (get-stack stack-machine)))
                         (top (get-stack stack-machine)))
                      (pop-exec-stack (pop-exec-stack stack-machine)))))

(define (truue? a) (false? (false? a)))

(define (POP_JUMP_IF_FALSE target stack-machine)
  (if (false? (top (get-stack stack-machine)))
      (JUMP_ABSOLUTE target stack-machine)
      (POP_TOP stack-machine)))

(define (POP_JUMP_IF_TRUE target stack-machine)
  (if (truue? (top (get-stack stack-machine)))
      (JUMP_ABSOLUTE target stack-machine)
      (POP_TOP stack-machine)))

(define (GET_ITER stack-machine) (incrementare_IC 1 stack-machine))

(define (FOR_ITER delta stack-machine)
  (if (null? (top (get-stack stack-machine)))
         (incrementare_IC (+ 2 (/ delta 2)) (pop-exec-stack stack-machine))
         (incrementare_IC 1 (push-exec-stack (car (top (get-stack stack-machine)))
                          (push-exec-stack (cdr (top (get-stack stack-machine))) (pop-exec-stack stack-machine))))))

         
(define (RETURN_VALUE stack-machine) (incrementare_IC 1 stack-machine))

(define (nume-operatie stack-machine) (car (list-ref (get-code stack-machine) (get-IC stack-machine))))
(define (arg-operatie stack-machine) (cdr (list-ref (get-code stack-machine) (get-IC stack-machine))))
        
(define (run-stack-machine stack-machine) 
  (cond
    ((>= (get-IC stack-machine) (length (get-code stack-machine))) stack-machine)
    ((equal? (nume-operatie stack-machine) 'POP_TOP) (run-stack-machine (POP_TOP stack-machine)))
    ((equal? (nume-operatie stack-machine) 'LOAD_CONST ) (run-stack-machine (LOAD_CONST (arg-operatie stack-machine) stack-machine)))
    ((equal? (nume-operatie stack-machine) 'LOAD_FAST ) (run-stack-machine (LOAD_FAST (arg-operatie stack-machine) stack-machine))) 
    ((equal? (nume-operatie stack-machine) 'STORE_FAST) (run-stack-machine (STORE_FAST (arg-operatie stack-machine) stack-machine)))
    ((equal? (nume-operatie stack-machine) 'BINARY_MODULO) (run-stack-machine (BINARY_MODULO stack-machine)))
    ((equal? (nume-operatie stack-machine) 'BINARY_ADD) (run-stack-machine (BINARY_ADD stack-machine)))
    ((equal? (nume-operatie stack-machine) 'BINARY_SUBTRACT) (run-stack-machine (BINARY_SUBTRACT stack-machine)))
    ((equal? (nume-operatie stack-machine) 'INPLACE_ADD) (run-stack-machine (INPLACE_ADD stack-machine)))
    ((equal? (nume-operatie stack-machine) 'INPLACE_SUBTRACT) (run-stack-machine (INPLACE_SUBTRACT stack-machine)))
    ((equal? (nume-operatie stack-machine) 'INPLACE_MODULO) (run-stack-machine (INPLACE_MODULO stack-machine)))
    ((equal? (nume-operatie stack-machine) 'JUMP_ABSOLUTE) (run-stack-machine (JUMP_ABSOLUTE (arg-operatie stack-machine) stack-machine)))
    ((equal? (nume-operatie stack-machine) 'COMPARE_OP) (run-stack-machine (COMPARE_OP (arg-operatie stack-machine) stack-machine)))
    ((equal? (nume-operatie stack-machine) 'POP_JUMP_IF_FALSE) (run-stack-machine (POP_JUMP_IF_FALSE (arg-operatie stack-machine) stack-machine)))
    ((equal? (nume-operatie stack-machine) 'POP_JUMP_IF_TRUE) (run-stack-machine (POP_JUMP_IF_TRUE (arg-operatie stack-machine) stack-machine)))
    ((equal? (nume-operatie stack-machine) 'GET_ITER) (run-stack-machine (GET_ITER stack-machine)))
    ((equal? (nume-operatie stack-machine) 'RETURN_VALUE) (run-stack-machine (RETURN_VALUE stack-machine)))
    ((equal? (nume-operatie stack-machine) 'FOR_ITER) (run-stack-machine (FOR_ITER (arg-operatie stack-machine) stack-machine)))
    ((equal? (nume-operatie stack-machine) 'LOAD_GLOBAL) (run-stack-machine (LOAD_GLOBAL (arg-operatie stack-machine) stack-machine)))
    (else (run-stack-machine (RETURN_VALUE stack-machine)))
    ))