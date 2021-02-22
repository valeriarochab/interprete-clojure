(require '[clojure.test :refer [is deftest run-tests]])


(deftest test-palabra-reservada?
  (is (= true (palabra-reservada? 'REM)))
  (is (= false (palabra-reservada? 'SPACE)))
  )

(deftest test-expandir-next
  (is (= '((PRINT 1) (NEXT A) (NEXT B)) (expandir-nexts (list '(PRINT 1) (list 'NEXT 'A (symbol ",") 'B)))))
  (is (= '((NEXT A) (NEXT B)) (expandir-nexts (list (list 'NEXT 'A (symbol ",") 'B)))))
  )

(deftest test-dar-error
  (is (= nil (dar-error 16 [:ejecucion-inmediata 4])))
  (is (= nil (dar-error "?ERROR DISK FULL" [:ejecucion-inmediata 4])))
  (is (= nil (dar-error 16 [100 3])))
  (is (= nil (dar-error "?ERROR DISK FULL" [100 3])))
  )

(deftest test-operador?
  (is (= true (operador? '+)))
  (is (= false (operador? (symbol "%"))))
  )

(deftest test-anular-invalidos
  (is (= '(IF X nil * Y < 12 THEN LET nil X = 0) (anular-invalidos '(IF X & * Y < 12 THEN LET ! X = 0))))
  )

(deftest test-variable-float?
  (is (= true (variable-float? 'X)))
  (is (= false (variable-float? 'X%)))
  (is (= false (variable-float? 'X$)))
  )

(deftest test-variable-integer?
  (is (= true (variable-integer? 'X%)))
  (is (= false (variable-integer? 'X)))
  (is (= false (variable-integer? 'X$)))
  )

(deftest test-variable-string?
  (is (= true (variable-string? 'X$)))
  (is (= false (variable-string? 'X)))
  (is (= false (variable-string? 'X%)))
  )

(deftest test-contar-sentencias
  (is (= 2 (contar-sentencias 10 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])))
  (is (= 1 (contar-sentencias 15 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])))
  (is (= 2 (contar-sentencias 20 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])))
  )

(deftest test-precedencia
  (is (= 1 (precedencia 'OR)))
  (is (= 2 (precedencia 'AND)))
  (is (= 6 (precedencia '*)))
  (is (= 7 (precedencia '-u)))
  (is (= 9 (precedencia 'MID$)))
  )


(deftest test-eliminar-cero-decimal
  (is (= 1.5 (eliminar-cero-decimal 1.5)))
  (is (= 1.5 (eliminar-cero-decimal 1.50)))
  (is (= 1 (eliminar-cero-decimal 1.0)))
  (is (= "A" (eliminar-cero-decimal 'A)))
  )

(deftest test-eliminar-cero-entero
  (is (= nil (eliminar-cero-entero nil)))
  (is (= "A" (eliminar-cero-entero 'A)))
  (is (= "0" (eliminar-cero-entero 0)))
  (is (= "1.5" (eliminar-cero-entero 1.5)))
  (is (= "1" (eliminar-cero-entero 1)))
  (is (= "-1" (eliminar-cero-entero -1)))
  (is (= "-1.5" (eliminar-cero-entero -1.5)))
  (is (= ".5" (eliminar-cero-entero 0.5)))
  (is (= "-.5" (eliminar-cero-entero -0.5)))
  )
(run-tests)