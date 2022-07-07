(ns tp1.core-test
  (:require [clojure.test :refer :all]
            [tp1.core :refer :all]))

;CONTROLAR-ARIDAD TESTS
(deftest controlar-aridad-test
          (testing "Prueba para la funcion controlar-aridad"
                   (is (= 3 (controlar-aridad '(a b c) 3)))
                   (is (= (list '*error* 'too-many-args) (controlar-aridad '(a b c) 2)))
                   (is (= (list '*error* 'too-few-args) (controlar-aridad '(a b c) 4)))
                   (is (= 0 (controlar-aridad '() 0)))
                   (is (= 3 (controlar-aridad '(a 2 B) 3)))))

;IGUAL TESTS
(deftest igual?-test
          (testing "Prueba la funcion igual?"
                   (is (= true (igual? 1 1)))
                   (is (= false (igual? 1 2)))
                   (is (= true (igual? 'a 'a)))
                   (is (= true (igual? 'A 'A)))
                   (is (= true (igual? 'a 'A)))
                   (is (= true (igual? 'A 'a)))
                   (is (= false (igual? 'a 'b)))
                   (is (= true (igual? '(a b c) '(A B C))))
                   (is (= false (igual? '(a b c) '(A B D))))
                   (is (= true (igual? nil nil)))
                   (is (= true (igual? nil 'NIL)))
                   (is (= true (igual? 'NIL nil)))
                   (is (= true (igual? 'NIL 'NIL)))
                   (is (= true (igual? nil ())))
                   (is (= true (igual? 'NIL () )))
                   (is (= true (igual? () () )))
                   (is (= true (igual? "a" "a")))
                   (is (= false (igual? "a" "A")))
                   (is (= false (igual? "a" "A")))
                   (is (= false (igual? 'a "A")))
                   (is (= true (igual? "tizziana" "tizziana")))
                   (is (= true (igual? 'tizziana 'tizziana)))))

;ERROR
(deftest error?-test
          (testing "Prueba la funcion error?"
                   (is (= true (error? '(*error* too-few-args))))
                   (is (= true (error? (list '*error* 'too-few-args))))
                   (is (= true (error? (list '*ERROR* 'too-few-args))))
                   (is (= true (error? (list '*Error* 'too-few-args))))
                   (is (= true (error? (list '*error*))))
                   (is (= false (error? (list 'too-few-args))))
                   (is (= false (error? '*error*)))
                   (is (= false (error? ())))
                   (is (= false (error? nil)))
                   (is (= true (error?  (list '*Error* 'tizziana))))))

;REVISAR-FN
(deftest revisar-fn-test
          (testing "Prueba la funcion revisar-fn"
                   (is (= (list '*error* 'too-few-args) (revisar-fnc '(*error* too-few-args))))
                   (is (= nil (revisar-fnc '(too-few-args))))
                   (is (= nil (revisar-fnc '*error*)))
                   (is (= nil (revisar-fnc nil)))
                   (is (= nil (revisar-fnc ())))))

;REVISAR-LAE
(deftest revisar-lae-test
          (testing "Prueba la funcion revisar-lae"
                   (is (= nil (revisar-lae '(1 2 3))))
                   (is (= nil (revisar-lae nil)))
                   (is (= nil (revisar-lae () )))
                   (is (= (list '*error* 'too-few-args) (revisar-lae '(1 (*error* too-few-args) 3))))
                   (is (= (list '*error* 'too-few-args) (revisar-lae '(1 (*error* too-few-args) (*error* too-many-args) 3))))))

;ACTUALIZAR-AMB
(deftest actualizar-amb-test
          (testing "Prueba la funcion actualizar-amb"
                   (is (= '(a 1 b 2 c 3 d 4) (actualizar-amb '(a 1 b 2 c 3) 'd 4)))
                   (is (= '(a 1 b 4 c 3) (actualizar-amb '(a 1 b 2 c 3) 'b 4)))
                   (is (= '(a 1 b 2 c 3)  (actualizar-amb '(a 1 b 2 c 3) 'b (list '*error* 'mal 'hecho))))
                   (is (= '(b 7)  (actualizar-amb '() 'b 7)))))

;;BUSCAR
(deftest buscar-test
          (testing "Prueba la funcion buscar"
                   (is (= 3 (buscar 'c '(a 1 b 2 c 3 d 4 e 5))))
                   (is (= (list '*error* 'unbound-symbol 'f) (buscar 'f '(a 1 b 2 c 3 d 4 e 5))))
                   (is (= 101715 (buscar 'padron '(nombre tizziana apellido mazza padron 101715))))
                   (is (= (list '*error* 'unbound-symbol 't) (buscar 't '(a 1 b 2 c 3 d 4 e 5))))))

;;FNC-APPEND
(deftest fnc-append-test
          (testing "Prueba la funcion fnc-append"
                   (is (= (list '*error* 'too-few-args) (fnc-append '( (1 2) ))))
                   (is (= (list '*error* 'too-many-args) (fnc-append '( (1 2) (3) (4 5) (6 7) ))))
                   (is (= (list '*error* 'list 'expected '3) (fnc-append '( (1 2) 3 ))))
                   (is (= (list '*error* 'list 'expected 'A) (fnc-append '( (1 2) A ))))
                   (is (= '(1 2 3) (fnc-append '( (1 2) (3) ))))
                   (is (= '(1 2) (fnc-append '( (1 2) nil ))))
                   (is (= '(1 2) (fnc-append '( () (1 2) ))))
                   (is (= nil (fnc-append '( nil nil ))))
                   (is (= nil (fnc-append '( () () ))))))

;;FNC-ENV
(deftest fnc-env-test
          (testing "Prueba la funcion fnc-env"
                   (is (= '(a 1 b 2 c 3 d 4) (fnc-env () '(a 1 b 2) '(c 3 d 4))))
                   (is (= (list '*error* 'too-many-args) (fnc-env '(5) '(a 1 b 2) '(c 3 d 4))))))

;;FNC-EQUAL
(deftest fnc-equal-test
          (testing "Prueba la funcion fnc-equal"
                   (is (= 't (fnc-equal '(1 1))))
                   (is (= 't (fnc-equal '(A a)) ))
                   (is (= 't (fnc-equal '("1" "1"))))
                   (is (= 't (fnc-equal '(nil NIL))))
                   (is (= nil (fnc-equal '(1 2))))
                   (is (= nil (fnc-equal '(A B)) ))
                   (is (= nil (fnc-equal '("1" 2))))
                   (is (= (list '*error* 'too-few-args) (fnc-equal '())))
                   (is (= (list '*error* 'too-many-args) (fnc-equal '(A a A))))))

;;FNC-READ
(deftest fnc-read-test
        (testing "Prueba la funcion read"
                  (is (= (with-in-str "1" (fnc-read ())) '1))
                  (is (= (with-in-str "a" (fnc-read ())) 'a))
                  (is (= (with-in-str "\"hola\"" (fnc-read ())) "hola"))
                  (is (= (with-in-str "(hola mundo)" (fnc-read ())) '(hola mundo)))
                  (is (= (with-in-str "(hola\nmundo)" (fnc-read ())) '(hola mundo)))
                  (is (= (with-in-str "()" (fnc-read ())) nil))
                  (is (= (with-in-str "nil" (fnc-read ())) nil))
                  (is (= (fnc-read '(1))) '(error not-implemented))
                  (is (= (fnc-read '(1 2))) '(error not-implemented))))



;;FNC-TERPRI
(deftest fnc-terpri-test
  (testing "Prueba la funcion fnc-terpri "
           (is (= (with-in-str "\n") (fnc-terpri ())))
           (is (= (list '*error* 'not-implemented) (fnc-terpri '(1))))
           (is (= (list '*error* 'not-implemented) (fnc-terpri '(1 2))))))

;;FNC-ADD
(deftest fnc-add-test
            (testing "Prueba la funcion fnc-add "
                     (is (= (list '*error* 'too-few-args) (fnc-add ())))
                     (is (= (list '*error* 'too-few-args) (fnc-add '(3))))
                     (is (= 7 (fnc-add '(3 4))))
                     (is (= 12 (fnc-add '(3 4 5))))
                     (is (= 18 (fnc-add '(3 4 5 6))))
                     (is (= (list '*error* 'number-expected 'A) (fnc-add '(A 4 5 6))))
                     (is (= (list '*error* 'number-expected 'A) (fnc-add '(3 A 5 6))))
                     (is (= (list '*error* 'number-expected 'A 'B) (fnc-add '(3 A B 6))))
                     (is (= (list '*error* 'number-expected 'A) (fnc-add '(3 4 A 6))))))

;;FNC-SUB
(deftest fnc-sub-test
           (testing "Prueba la funcion fnc-sub"
                    (is (= (list '*error* 'too-few-args) (fnc-sub ())))
                    (is (= -3 (fnc-sub '(3))))
                    (is (= -1 (fnc-sub '(3 4))))
                    (is (= -6 (fnc-sub '(3 4 5))))
                    (is (= -12 (fnc-sub '(3 4 5 6))))
                    (is (= (list '*error* 'number-expected 'A) (fnc-sub '(A 4 5 6))))
                    (is (= (list '*error* 'number-expected 'A) (fnc-sub '(3 A 5 6))))
                    (is (= (list '*error* 'number-expected 'A) (fnc-sub '(3 4 A 6))))))

;;FNC-LT
(deftest fnc-lt-test
           (testing "Prueba la funcion fnc-lt"
                    (is (= (list '*error* 'too-few-args) (fnc-lt ())))
                    (is (= (list '*error* 'too-few-args)  (fnc-lt '(1))))
                    (is (= 't (fnc-lt '(1 2))))
                    (is (= nil (fnc-lt '(1 1))))
                    (is (= (list '*error* 'number-expected 'A) (fnc-lt '(A 1))))
                    (is (= (list '*error* 'number-expected 'A) (fnc-lt '(1 A ))))
                    (is (= (list '*error* 'too-many-args)  (fnc-lt '(1 2 3))))))

;;FNC-GT
(deftest fnc-gt-test
          (testing "Prueba la funcion fnc-gt"
                   (is (= (list '*error* 'too-few-args) (fnc-gt ())))
                   (is (= (list '*error* 'too-few-args)  (fnc-gt '(1))))
                   (is (= 't (fnc-gt '(2 1))))
                   (is (= nil (fnc-gt '(1 1))))
                   (is (= (list '*error* 'number-expected 'A) (fnc-gt '(A 1))))
                   (is (= (list '*error* 'number-expected 'A) (fnc-gt '(1 A ))))
                   (is (= (list '*error* 'too-many-args)  (fnc-gt '(1 2 3))))))

;;FNC-GE
(deftest fnc-ge-test
          (testing "Prueba la funcion fnc-ge"
                   (is (= (list '*error* 'too-few-args) (fnc-ge ())))
                   (is (= (list '*error* 'too-few-args)  (fnc-ge '(1))))
                   (is (= 't (fnc-ge '(1 1))))
                   (is (= 't (fnc-ge '(2 1))))
                   (is (= nil (fnc-ge '(1 2))))
                   (is (= (list '*error* 'number-expected 'A) (fnc-ge '(A 1))))
                   (is (= (list '*error* 'number-expected 'A) (fnc-ge '(1 A ))))
                   (is (= (list '*error* 'too-many-args)  (fnc-ge '(1 2 3))))))

;;FNC-REVERSE
(deftest fnc-reverse-test
          (testing "Prueba la funcion fnc-reverse"
                   (is (= (list '*error* 'too-few-args) (fnc-reverse ())))
                   (is (= (list '*error* 'list 'expected 1)  (fnc-reverse '(1))))
                   (is (= (list '*error* 'list 'expected 'A)  (fnc-reverse '(A))))
                   (is (= (list 1) (fnc-reverse '((1)) )))
                   (is (= (list 3 2 1) (fnc-reverse '((1 2 3)) )))
                   (is (= (list '*error* 'too-many-args)  (fnc-reverse '((1 2 3)(4)))))))

;;EVALUAR-ESCALAR
(deftest evaluar-escalar-test
          (testing "Prueba la funcion evaluar-escalar"
                   (is (= '(32 (v 1 w 3 x 6)) (evaluar-escalar 32 '(v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
                   (is (= '("chau" (v 1 w 3 x 6)) (evaluar-escalar "chau" '(v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
                   (is (= '("hola" (v 1 w 3 x 6)) (evaluar-escalar 'z '(v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
                   (is (= '("hola" (v 1 w 3 x 6)) (evaluar-escalar 'Z '(v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
                   (is (= '(3 (v 1 w 3 x 6)) (evaluar-escalar 'w '(v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
                   (is (= '(5 (v 1 w 3 x 6)) (evaluar-escalar 'x '(v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
                   (is (= (list (list '*error* 'unbound-symbol 'n) '(v 1 w 3 x 6)) (evaluar-escalar 'n '(v 1 w 3 x 6) '(x 5 y 11 z "hola"))))))

;;EVALUAR-DE
(deftest evaluar-de-test
          (testing "Prueba la funcion evaluar-de"
                   (is (= '(f (x 1 f (lambda (x)))) (evaluar-de '(de f (x)) '(x 1))))
                   (is (= '(f (x 1 f (lambda (x) 2))) (evaluar-de '(de f (x) 2) '(x 1))))
                   (is (= '(f (x 1 f (lambda (x) (+ x 1)))) (evaluar-de '(de f (x) (+ x 1)) '(x 1))))
                   (is (= '(f (x 1 f (lambda (x y) (+ x y)))) (evaluar-de '(de f (x y) (+ x y)) '(x 1))))
                   (is (= '(f (x 1 f (lambda (x y) (prin3 x) (terpri) y))) (evaluar-de '(de f (x y) (prin3 x) (terpri) y) '(x 1))))
                   (is (= (list (list '*error* 'list 'expected nil) '(x 1)) (evaluar-de '(de) '(x 1))))
                   (is (= (list (list '*error* 'list 'expected nil) '(x 1)) (evaluar-de '(de f) '(x 1))))
                   (is (= (list (list '*error* 'list 'expected '2) '(x 1)) (evaluar-de '(de f 2) '(x 1))))
                   (is (= (list (list '*error* 'list 'expected '2) '(x 1)) (evaluar-de '(de f 2 3) '(x 1))))
                   (is (= (list (list '*error* 'list 'expected nil) '(x 1)) (evaluar-de '(de (f)) '(x 1))))
                   (is (= (list (list '*error* 'list 'expected 'x) '(x 1)) (evaluar-de '(de 2 x) '(x 1))))
                   (is (= (list (list '*error* 'symbol 'expected 2) '(x 1)) (evaluar-de '(de 2 (x)) '(x 1))))
                   (is (= (list (list '*error* 'cannot-set nil) '(x 1)) (evaluar-de '(de nil (x) 2) '(x 1))))))

;;EVALUAR-IF
(deftest evaluar-if-test
         (testing "Prueba la funcion evaluar-if"
                  (is (= '(nil (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if t) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
                  (is (= '(nil (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if 7) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
                  (is (= '(nil (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if nil) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
                  (is (= '(nil (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if x) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
                  (is (= '(9 (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if t 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
                  (is (= '(9 (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if z 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
                  (is (= '(9 (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if w 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
                  (is (= (list (list '*error* 'unbound-symbol 'r) '(nil nil t t v 1 w 3 x 6)) (evaluar-if '(if r 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
                  (is (= '(nil (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if nil 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
                  (is (= '("hola" (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if nil 9 z) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
                  (is (= '("hola" (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if nil 9 1 2 3 z) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
                  (is (= '(3 (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if nil 9 w) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
                  (is (= '(8 (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if nil 9 8) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
                  (is (= (list (list '*error* 'unbound-symbol 'a) '(gt gt nil nil t t v 1 w 3 x 6)) (evaluar-if '(if (gt 2 0) a 8) '(gt gt nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
                  (is (= '(8 (gt gt nil nil t t v 1 w 3 x 6)) (evaluar-if '(if (gt 0 2) a 8) '(gt gt nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
                  (is (= '(8 (gt gt nil nil t t v 1 w 3 x 6 m 8)) (evaluar-if '(if (gt 0 2) a (setq m 8)) '(gt gt nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))))

;EVALURAR-OR
(deftest evaluar-or-test
        (testing "Prueba la funcion evaluar-or"
                 (is (= '(nil (nil nil t t w 5 x 4)) (evaluar-or '(or) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))))
                 (is (= '(nil (nil nil t t w 5 x 4)) (evaluar-or '(or nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))))
                 (is (= '(t (nil nil t t w 5 x 4)) (evaluar-or '(or t) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))))
                 (is (= '(5 (nil nil t t w 5 x 4)) (evaluar-or '(or w) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))))
                 (is (= (list (list '*error* 'unbound-symbol 'r) '(nil nil t t w 5 x 4)) (evaluar-or '(or r) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))))
                 (is (= '(nil (nil nil t t w 5 x 4)) (evaluar-or '(or y) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))))
                 (is (= '(6 (nil nil t t w 5 x 4)) (evaluar-or '(or 6) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))))
                 (is (= '(6 (nil nil t t w 5 x 4)) (evaluar-or '(or nil 6) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))))
                 (is (= '(6 (nil nil t t w 5 x 4)) (evaluar-or '(or nil 6 nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))))
                 (is (= '(6 (nil nil t t w 5 x 4)) (evaluar-or '(or nil 6 r nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))))
                 (is (= '(t (nil nil t t w 5 x 4)) (evaluar-or '(or nil t r nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))))
                 (is (= '(nil (nil nil t t w 5 x 4)) (evaluar-or '(or nil nil nil nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))))
                 (is (= '(8 (nil nil t t w 5 x 4 b 8)) (evaluar-or '(or (setq b 8) nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))))
                 ))

;EVALUAR-SETQ
(deftest evaluar-setq-test
         (testing "Prueba la funcion evaluar-setq"
                  (is (= (list(list '*error* 'list 'expected nil) '(nil nil t t + add w 5 x 4)) (evaluar-setq '(setq) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3))))
                  (is (= (list(list '*error* 'list 'expected nil) '(nil nil t t + add w 5 x 4)) (evaluar-setq '(setq m) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3))))
                  (is (= '(7 (nil nil t t + add w 5 x 4 m 7)) (evaluar-setq '(setq m 7) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3))))
                  (is (= '(7 (nil nil t t + add w 5 x 7)) (evaluar-setq '(setq x 7) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3))))
                  (is (= '(2 (nil nil t t + add w 5 x 2)) (evaluar-setq '(setq x (+ x 1)) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3))))
                  (is (= '(5 (nil nil t t + add w 5 x 5)) (evaluar-setq '(setq x (+ x 1)) '(nil nil t t + add w 5 x 4) '(y nil z 3))))
                  (is (= (list(list '*error* 'list 'expected nil) '(nil nil t t + add w 5 x 4)) (evaluar-setq '(setq nil) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3))))
                  (is (= (list(list '*error* 'cannot-set nil) '(nil nil t t + add w 5 x 4)) (evaluar-setq '(setq nil 7) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3))))
                  (is (= (list(list '*error* 'symbol 'expected 7) '(nil nil t t + add w 5 x 4)) (evaluar-setq '(setq 7 8) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3))))
                  (is (= '(8 (nil nil t t + add w 5 x 7 m 8)) (evaluar-setq '(setq x 7 m (+ x 7)) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3))))
                  (is (= '(14 (nil nil t t + add w 5 x 7 m 14)) (evaluar-setq '(setq x 7 m (+ x 7)) '(nil nil t t + add w 5 x 4) '(y nil z 3))))
                  (is (= (list(list '*error* 'list 'expected nil) '(nil nil t t + add w 5 x 7)) (evaluar-setq '(setq x 7 y) '(nil nil t t + add w 5 x 4) '(y nil z 3))))
                  (is (= '(9 (nil nil t t + add w 5 x 7 y 8 z 9)) (evaluar-setq '(setq x 7 y 8 z 9) '(nil nil t t + add w 5 x 4) '(y nil z 3))))))