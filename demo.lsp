(terpri)
(prin3 "*****************************************************") (terpri)
(prin3 "*                  TLC-LISP 2022                    *") (terpri)
(prin3 "* DEMO DE DEFINICION Y USO DE VARIABLES Y FUNCIONES *") (terpri)
(prin3 "*****************************************************") (terpri)
(terpri)
(prin3 "OBS.: TLC-LISP NO DISTINGUE MAYUSCULAS DE MINUSCULAS.") (terpri)
(prin3 "      PARA APROVECHAR ESTA CARACTERISTICA, ALGUNAS   ") (terpri)
(prin3 "      VARIABLES Y FUNCIONES SE DEFINIERON A PROPOSITO") (terpri)
(prin3 "      EN MAYUSCULAS Y OTRAS EN MINUSCULAS.")            (terpri)

(terpri) (prin3 "Type a letter or a digit and press Enter... ") (read)

(terpri)
(prin3 "DEFINICION DE VARIABLES") (terpri)
(prin3 "-----------------------") (terpri)

(prin3 "> (setq u 'u)") (terpri)
(prin3 (setq u 'u)) (terpri)

(prin3 "> (setq v 'v)") (terpri)
(prin3 (setq v 'v)) (terpri)

(prin3 "> (setq w 'w)") (terpri)
(prin3 (setq w 'w)) (terpri)

(terpri)
(prin3 "LAS VARIABLES AHORA ESTAN EN EL AMBIENTE.") (terpri)
(prin3 "EVALUANDOLAS SE OBTIENEN SUS VALORES:") (terpri)
(prin3 "> u") (terpri)
(prin3 u) (terpri)
(prin3 "> v") (terpri)
(prin3 v) (terpri)
(prin3 "> w") (terpri)
(prin3 w) (terpri)

(terpri) (prin3 "Type a letter or a digit and press Enter... ") (read)

(terpri)
(prin3 "UNA VEZ DEFINIDA UNA VARIABLE, CON SETQ TAMBIEN") (terpri)
(prin3 "SE LE PUEDE CAMBIAR EL VALOR:") (terpri)

(prin3 "> (setq n 0)") (terpri)
(prin3 (setq n 0)) (terpri)

(prin3 "> (setq N 17)") (terpri)
(prin3 (setq N 17)) (terpri)

(prin3 "> n") (terpri)
(prin3 n) (terpri)

(terpri) (prin3 "Type a letter or a digit and press Enter... ") (read)

(terpri)
(prin3 "DEFINICION DE FUNCIONES") (terpri)
(prin3 "-----------------------") (terpri)

(prin3 "> (de sumar (a b) (add a b))") (terpri)
(prin3 (de sumar (a b) (add a b))) (terpri)

(prin3 "> (de restar (a b) (sub a b))") (terpri)
(prin3 (de restar (a b) (sub a b))) (terpri)

(terpri)
(prin3 "LAS FUNCIONES AHORA ESTAN EN EL AMBIENTE.") (terpri)
(prin3 "ES POSIBLE APLICARLAS A VALORES FORMANDO EXPRESIONES") (terpri)
(prin3 "QUE EVALUADAS GENERAN RESULTADOS:") (terpri)

(prin3 "> (sumar 3 5)") (terpri)
(prin3 (sumar 3 5)) (terpri)

(prin3 "> (restar 12 5)") (terpri)
(prin3 (restar 12 5)) (terpri)

(terpri) (prin3 "Type a letter or a digit and press Enter... ") (read)

(terpri)
(prin3 "TLC-LISP ES UN LENGUAJE DE AMBITO DINAMICO (DYNAMICALLY SCOPED):") (terpri)

(prin3 "> (setq x 1)") (terpri)
(prin3 (setq x 1)) (terpri)

(prin3 "> (de g (y) (+ x y))") (terpri)
(prin3 (de g (y) (+ x y))) (terpri)

(prin3 "> (de f (x) (g 2))") (terpri)
(prin3 (de f (x) (g 2))) (terpri)

(prin3 "> (f 5)") (terpri)
(prin3 (f 5)) (terpri)
(prin3 "[En Scheme -lexically scoped- daria 3 en lugar de 7.]") (terpri)

(terpri) (prin3 "Type a letter or a digit and press Enter... ") (read)

(terpri)
(prin3 "APLICACION DE FUNCIONES ANONIMAS [LAMBDAS]") (terpri)
(prin3 "------------------------------------------") (terpri)

(terpri)
(prin3 "LAMBDA CON CUERPO SIMPLE:") (terpri)
(prin3 "> ((lambda (y) (+ 1 y)) 15)") (terpri)
(prin3 ((lambda (y) (+ 1 y)) 15)) (terpri)

(terpri)
(prin3 "LAMBDA CON CUERPO MULTIPLE:") (terpri)
(prin3 "> ((lambda (y) (prin3 'Hola!) (terpri) (+ 1 y)) 5)") (terpri)
(prin3 ((lambda (y) (prin3 'Hola!) (terpri) (+ 1 y)) 5)) (terpri)

(terpri)
(prin3 "LAMBDA CON CUERPO MULTIPLE Y EFECTOS COLATERALES [SIDE EFFECTS]:") (terpri)
(prin3 "> ((lambda (a b c) (setq u a) (setq v b) (setq w c)) 1 2 3)") (terpri)
(prin3 ((lambda (a b c) (setq u a) (setq v b) (setq w c)) 1 2 3)) (terpri)
(terpri)

(prin3 "LOS NUEVOS VALORES DE LAS VARIABLES MODIFICADAS:") (terpri)
(prin3 "> u") (terpri)
(prin3 u) (terpri)
(prin3 "> v") (terpri)
(prin3 v) (terpri)
(prin3 "> w") (terpri)
(prin3 w) (terpri)

(terpri) (prin3 "Type a letter or a digit and press Enter... ") (read)

(terpri)
(prin3 "APLICACION PARCIAL:") (terpri)
(prin3 "> (((lambda (x) (lambda (y) (- x y))) 8) 3)") (terpri)
(prin3 (((lambda (x) (lambda (y) (- x y))) 8) 3)) (terpri)

(terpri)
(prin3 "EL MISMO EJEMPLO ANTERIOR, AHORA DEFINIENDO UNA FUNCION:") (terpri)
(prin3 "> (setq p (lambda (x) (lambda (y) (- x y))))") (terpri)
(prin3 (setq p (lambda (x) (lambda (y) (- x y))))) (terpri)

(prin3 "> (p 8)") (terpri)
(prin3 (p 8)) (terpri)

(prin3 "> ((p 8) 3)") (terpri)
(prin3 ((p 8) 3)) (terpri)

(terpri) (prin3 "Type a letter or a digit and press Enter... ") (read)

(terpri)
(prin3 "DEFINICION DE FUNCIONES RECURSIVAS [RECORRIDO LINEAL]") (terpri)
(prin3 "-----------------------------------------------------") (terpri)

(terpri)
(prin3 "FUNCION RECURSIVA CON EFECTO COLATERAL") (terpri)
(prin3 "[DEJA EN LA VARIABLE D LA CANTIDAD DE PARES]:") (terpri)

(prin3 "> (de recorrer (L)") (terpri)
(prin3 "    (recorrer2 L 0))") (terpri)

(prin3
(de recorrer (L)
  (recorrer2 L 0))
) (terpri)
  
(prin3 "> (setq D 0)") (terpri)
(prin3 (setq D 0)) (terpri)

(prin3 "> (de recorrer2 (L i)") (terpri)
(prin3 "    (cond") (terpri)
(prin3 "      ((null (rest L)) (setq D (+ 1 D)) (list (first L) i))") (terpri)
(prin3 "      (t (prin3 (list (first L) i)) (setq D (+ i 1)) (terpri) (recorrer2 (rest L) D))))") (terpri)

(prin3
(de recorrer2 (L i)
  (cond
    ((null (rest L)) (setq D (+ 1 D)) (list (first L) i))
    (t (prin3 (list (first L) i)) (setq D (+ i 1)) (terpri) (recorrer2 (rest L) D))))
) (terpri)

(prin3 "> (recorrer '(x y z))") (terpri)
(prin3 (recorrer '(x y z))) (terpri)

(prin3 "> d") (terpri)
(prin3 d) (terpri)

(terpri) (prin3 "Type a letter or a digit and press Enter... ") (read)

(terpri)
(prin3 "DEFINICION DE FUNCIONES RECURSIVAS [RECORRIDO A TODO NIVEL]") (terpri)
(prin3 "-----------------------------------------------------------") (terpri)

(terpri)
(prin3 "EXISTENCIA DE UN ELEMENTO ESCALAR EN UNA LISTA:") (terpri)

(prin3 "> (DE EXISTE (A L)") (terpri)
(prin3 "    (COND") (terpri)
(prin3 "      ((NULL L) NIL)") (terpri)
(prin3 "      ((NOT (LISTP (FIRST L))) (OR (EQUAL A (FIRST L)) (EXISTE A (REST L))))") (terpri)
(prin3 "      (T (OR (EXISTE A (FIRST L)) (EXISTE A (REST L))))))") (terpri)

(PRIN3
(de EXISTE (A L)
  (COND
    ((NULL L) NIL)
    ((NOT (listp (FIRST L))) (OR (EQUAL A (FIRST L)) (EXISTE A (REST L))))
    (T (OR (EXISTE A (FIRST L)) (EXISTE A (REST L))))))
) (TERPRI)

(prin3 "> (existe 'c '(a ((b) ((d c) a) e f)))") (terpri)
(prin3 (existe 'c '(a ((b) ((d c) a) e f)))) (terpri)

(prin3 "> (existe 'g '(a ((b) ((d c) a) e f)))") (terpri)
(prin3 (existe 'g '(a ((b) ((d c) a) e f)))) (terpri)

(terpri) (prin3 "Type a letter or a digit and press Enter... ") (read)

(terpri)
(prin3 "ELIMINACION DE UN ELEMENTO DE UNA LISTA:") (terpri)

(prin3 "> (de eliminar (dat li)") (terpri)
(prin3 "    (cond") (terpri)
(prin3 "      ((null li) li)") (terpri)
(prin3 "      ((equal dat (first li)) (eliminar dat (rest li)))") (terpri)
(prin3 "      ((listp (first li)) (cons (eliminar dat (first li)) (eliminar dat (rest li))))") (terpri)
(prin3 "      (T (cons (first li) (eliminar dat (rest li))))))") (terpri)

(prin3
(de eliminar (dat li)
  (cond
    ((null li) li)
    ((equal dat (first li)) (eliminar dat (rest li)))
    ((listp (first li)) (cons (eliminar dat (first li)) (eliminar dat (rest li))))
    (T (cons (first li) (eliminar dat (rest li))))))
) (terpri)

(prin3 "> (eliminar 'c '(a ((b) ((d c) a) c f)))") (terpri)
(prin3 (eliminar 'c '(a ((b) ((d c) a) c f)))) (terpri)

(prin3 "> (eliminar '(1 2 3) '(a ((b) (((1 2 3) c) a) c f)))") (terpri)
(prin3 (eliminar '(1 2 3) '(a ((b) (((1 2 3) c) a) c f)))) (terpri)

(terpri) (prin3 "Type a letter or a digit and press Enter... ") (read)

(terpri)
(prin3 "PROFUNDIDAD DE UNA LISTA:") (terpri)

(prin3 "> (de profundidad (lista)") (terpri)
(prin3 "    (if (or (not (listp lista)) (null lista)) 0") (terpri)
(prin3 "        (if (gt (+ 1 (profundidad (first lista))) (profundidad (rest lista)))") (terpri)
(prin3 "            (+ 1 (profundidad (first lista)))") (terpri)
(prin3 "            (profundidad (rest lista)))))") (terpri)

(prin3
(de profundidad (lista)
  (if (or (not (listp lista)) (null lista)) 0
      (if (gt (+ 1 (profundidad (first lista))) (profundidad (rest lista)))
          (+ 1 (profundidad (first lista)))
          (profundidad (rest lista)))))
) (terpri)

(prin3 "> (profundidad '((2 3)(3 ((7))) 5))") (terpri)
(prin3 (profundidad '((2 3)(3 ((7))) 5))) (terpri)
(prin3 "[El valor esperado es 4.]") (terpri)

(terpri) (prin3 "Type a letter or a digit and press Enter... ") (read)

(terpri)
(prin3 "PLANCHADO DE UNA LISTA:") (terpri)

(prin3 "> (de planchar (li)") (terpri)
(prin3 "    (cond") (terpri)
(prin3 "      ((null li) ())") (terpri)
(prin3 "      ((listp (first li)) (append (planchar (first li)) (planchar (rest li))))") (terpri)
(prin3 "      (T (cons (first li) (planchar (rest li))))))") (terpri)

(prin3
(de planchar (li)
  (cond
    ((null li) ())
    ((listp (first li)) (append (planchar (first li)) (planchar (rest li))))
    (T (cons (first li) (planchar (rest li))))))
) (terpri)

(prin3 "> (planchar '((2 3)(3 ((7))) 5))") (terpri)
(prin3 (planchar '((2 3)(3 ((7))) 5))) (terpri)

(terpri) (prin3 "Type a letter or a digit and press Enter... ") (read)

(terpri)
(prin3 "DEFINICION DE FUNCIONES PARA OCULTAR LA RECURSIVIDAD") (terpri)
(prin3 "----------------------------------------------------") (terpri)

(terpri)
(prin3 "FILTRAR [SELECCIONA LOS ELEMENTOS QUE CUMPLAN UNA CONDICION DADA]:") (terpri)

(prin3 "> (de FILTRAR (F L)") (terpri)
(prin3 "    (COND") (terpri)
(prin3 "      ((null L) ())") (terpri)
(prin3 "      ((F (first L)) (CONS (first L) (FILTRAR F (rest L))))") (terpri)
(prin3 "      (T (FILTRAR F (rest L)))))") (terpri)

(prin3
(de FILTRAR (F L)
  (COND
    ((null L) ())
    ((F (first L)) (CONS (first L) (FILTRAR F (rest L))))
    (T (FILTRAR F (rest L)))))
) (terpri)

(prin3 "> (filtrar (lambda (x) (gt x 0)) '(5 0 2 -1 4 6 0 8))") (terpri)
(prin3 (filtrar (lambda (x) (gt x 0)) '(5 0 2 -1 4 6 0 8))) (terpri)

(terpri) (prin3 "Type a letter or a digit and press Enter... ") (read)

(terpri)
(prin3 "REDUCIR [REDUCE UNA LISTA APLICANDO DE A PARES UNA FUNCION DADA]:") (terpri)

(prin3 "> (de REDUCIR (F L)") (terpri)
(prin3 "    (IF (null (rest L))") (terpri)
(prin3 "        (first L)") (terpri)
(prin3 "        (F (first L) (REDUCIR F (rest L)))))") (terpri)

(prin3
(de REDUCIR (F L)
  (IF (null (rest L))
      (first L)
      (F (first L) (REDUCIR F (rest L)))))
) (terpri)

(prin3 "> (reducir (lambda (x y) (if (gt x 0) (cons x y) y)) '(5 0 2 -1 4 6 0 8 ()))") (terpri)
(prin3 (reducir (lambda (x y) (if (gt x 0) (cons x y) y)) '(5 0 2 -1 4 6 0 8 ()))) (terpri)

(terpri) (prin3 "Type a letter or a digit and press Enter... ") (read)

(terpri)
(prin3 "MAPEAR [APLICA A CADA ELEMENTO DE UNA LISTA UNA FUNCION DADA]:") (terpri)

(prin3 "> (de MAPEAR (OP L)") (terpri)
(prin3 "    (IF (null L)") (terpri)
(prin3 "        ()") (terpri)
(prin3 "        (CONS (OP (first L)) (MAPEAR OP (rest L)))))") (terpri)

(prin3
(de MAPEAR (OP L)
  (IF (null L)
      ()
      (CONS (OP (first L)) (MAPEAR OP (rest L)))))
) (terpri)

(prin3 "> (mapear (lambda (x) (if (equal x 0) 'Z x)) '(5 0 2 -1 4 6 0 8))") (terpri)
(prin3 (mapear (lambda (x) (if (equal x 0) 'Z x)) '(5 0 2 -1 4 6 0 8))) (terpri)

(terpri) (prin3 "Type a letter or a digit and press Enter... ") (read)

(terpri)
(prin3 "TRANSPONER [TRANSPONE UNA LISTA DE LISTAS]:") (terpri)

(prin3 "> (de TRANSPONER (M)") (terpri)
(prin3 "    (IF (null (first M))") (terpri)
(prin3 "        ()") (terpri)
(prin3 "        (CONS (MAPEAR first M) (TRANSPONER (MAPEAR rest M)))))") (terpri)

(prin3
(de TRANSPONER (M)
  (IF (null (first M))
    ()
    (CONS (MAPEAR first M) (TRANSPONER (MAPEAR rest M)))))
) (terpri)

(prin3 "> (transponer '((a b c) (d e f) (g h i)))") (terpri)
(prin3 (transponer '((a b c) (d e f) (g h i)))) (terpri)

(terpri) (prin3 "Type a letter or a digit and press Enter... ") (read)

(terpri)
(prin3 "IOTA [RETORNA UNA LISTA CON LOS PRIMEROS N NUMEROS NATURALES]:") (terpri)

(prin3 "> (de IOTA (N)") (terpri)
(prin3 "    (IF (LT N 1)") (terpri)
(prin3 "         ()") (terpri)
(prin3 "         (AUXIOTA 1 N)))") (terpri)

(prin3
(de IOTA (N)
    (IF (LT N 1)
     ()
     (AUXIOTA 1 N)))
) (terpri)

(prin3 "> (de AUXIOTA (I N)") (terpri)
(prin3 "    (IF (EQUAL I N)") (terpri)
(prin3 "        (LIST N)") (terpri)
(prin3 "        (CONS I (AUXIOTA (+ I 1) N))))") (terpri)

(prin3
(de AUXIOTA (I N)
  (IF (EQUAL I N)
    (LIST N)
    (CONS I (AUXIOTA (+ I 1) N))))
) (terpri)

(prin3 "> (IOTA 10)") (terpri)
(prin3 (IOTA 10)) (terpri)

(terpri) (prin3 "Type a letter or a digit and press Enter... ") (read)

(terpri)
(prin3 "FUNCIONES IMPLEMENTADAS USANDO LAS FUNCIONES ANTERIORES") (terpri)
(prin3 "-------------------------------------------------------") (terpri)

(terpri)
(prin3 "SUMATORIA DE LOS PRIMEROS N NUMEROS NATURALES:") (terpri)

(prin3 "> (de sumatoria (n) (reducir + (iota n)))") (terpri)

(prin3
(de sumatoria (n) (reducir + (iota n)))
) (terpri)

(prin3 "> (sumatoria 10)") (terpri)
(prin3 (sumatoria 10)) (terpri)
(prin3 "[El valor esperado es 55.]") (terpri)

(terpri) (prin3 "Type a letter or a digit and press Enter... ") (read)

(terpri)
(prin3 "ELIMINACION DE LOS ELEMENTOS REPETIDOS EN UNA LISTA SIMPLE:") (terpri)

(prin3 "> (de eliminar-repetidos (li)") (terpri)
(prin3 "    (reverse (reducir (lambda (x y) (if (existe x y) y (cons x y))) (reverse (cons () li)))))") (terpri)

(prin3
(de eliminar-repetidos (li)
  (reverse (reducir (lambda (x y) (if (existe x y) y (cons x y))) (reverse (cons () li)))))
) (terpri)

(prin3 "> (eliminar-repetidos '(a b c d e f g d c h b i j))") (terpri)
(prin3 (eliminar-repetidos '(a b c d e f g d c h b i j))) (terpri)

(terpri) (prin3 "Type a letter or a digit and press Enter... ") (read)

(terpri)
(prin3 "SELECCION DEL ENESIMO ELEMENTO DE UNA LISTA DADA:") (terpri)

(prin3 "> (de seleccionar (n li)") (terpri)
(prin3 "    (if (or (lt n 1) (gt n (length li)))") (terpri)
(prin3 "        ()") (terpri)
(prin3 "        (first (first (filtrar (lambda (x) (equal n (first (rest x)))) (transponer (list li (iota (length li)))))))))") (terpri)

(prin3
(de seleccionar (n li)
  (if (or (lt n 1) (gt n (length li)))
      ()
      (first (first (filtrar (lambda (x) (equal n (first (rest x)))) (transponer (list li (iota (length li)))))))))
) (terpri)

(prin3 "> (SELECCIONAR 5 '(A B C D E F G H I J))") (terpri)
(prin3 (SELECCIONAR 5 '(A B C D E F G H I J))) (terpri)

(terpri) (prin3 "Type a letter or a digit and press Enter... ") (read)

(terpri)
(prin3 "APLICACION DE TODAS LAS FUNCIONES DE UNA LISTA A UN ELEMENTO DADO:") (terpri)

(prin3 "> (de aplicar-todas (lf x)") (terpri)
(prin3 "    (mapear (lambda (f) (f x)) lf))") (terpri)

(prin3
(de aplicar-todas (lf x)
  (mapear (lambda (f) (f x)) lf))
) (terpri)

(prin3 "> (aplicar-todas (list length rest first) '((3 2 1)(9 8)(7 6)(5 4)))") (terpri)
(prin3 (aplicar-todas (list length rest first) '((3 2 1)(9 8)(7 6)(5 4)))) (terpri)

(terpri) (prin3 "Type a letter or a digit and press Enter... ") (read)

(terpri)
(prin3 "ENTRADA DE DATOS Y SALIDA DEL INTERPRETE") (terpri)
(prin3 "----------------------------------------") (terpri)

(terpri)
(prin3 "CARGA DE DATOS DESDE LA TERMINAL/CONSOLA:") (terpri)
(prin3 "> (setq R 0)") (terpri)
(prin3 "> (de cargarR)") (terpri)
(prin3 "    (prin3 '->R: )(setq R (read))(prin3 'R*2: )(prin3 (+ R R))(terpri))") (terpri)
(prin3 "> (cargarR)") (terpri)

(setq R 0)
(de cargarR()
  (prin3 "->R: ")(setq R (read))(prin3 "R*2: ")(prin3 (+ R R))(terpri))
(cargarR)

(terpri)
(prin3 "PARA VER EL AMBIENTE [NO FUNCIONA EN TLC-LISP]: (env)") (terpri)
(prin3 "PARA SALIR DEL INTERPRETE: (exit)") (terpri)

'Carga-exitosa-de-demo-lsp