#|

Practica 04

Integrantes:
Hernandez Dorantes Israel - 318206604
Martinez Calzada Diego -  318275457

|#
#lang plai

;; ******************************************************************

;; Tipo de dato para representar el Arbol de Sintaxis Abstracta (AST)
(define-type AST
  [id (i symbol?)]
  [num (n number?)]
  [bool (b boolean?)]
  [op (f procedure?) (args (listof AST?))]
  [op-bool (f symbol?) (larg AST?) (rarg AST?)]
  [with (bindings (listof binding?)) (body AST?)]
  [with* (bindings (listof binding?)) (body AST?)]
  [fun (params (listof symbol?)) (body AST?)]
  [lempty]
  [lcons (l AST?) (r AST?)]
  [lcar (lst AST?)]
  [lcdr (lst AST?)]
  [app (fun AST?) (args (listof AST?))]
)

;; Tipo de dato para las ataduras (Bindings)
(define-type Binding
  [binding (id symbol?) (value AST?)]
)

;; Tipo de dato para los ambientes (Environment)
(define-type Environment
  [mtSub]
  [aSub (name symbol?) (value AST?) (bSub Environment?)])

;; Tipo de dato para los valores FWAEL
(define-type FWAEL-Value
  [numV (n number?)]
  [boolV (b boolean?)]
  [listV (l (listof FWAEL-Value?))]
  [closureV (param (listof symbol?)) (body AST?) (env Environment?)])

;; ******************************************************************

;; 1. (2 pts). Funcion que cual recibe una expresion simbolica (symbolic expression,
;; s-expression); y construye un Arbol de Sintaxis Abstracta (Abstract Syntax Tree - AST) a
;; partir de la s-expression dada; si es una expresión válida en el lenguaje FWAEL.
;; En otro caso, arroja un error.
;; * Precondiciones: una expresion simbolica (symbolic expression; s-expression).
;; * Postcondiciones: si la s-expression de entrada satisface la gramática del lenguaje FWAEL;
;;   devuelve un arbol de sintaxis abstracta AST, de lo contrario, envia un error.
;; parse: s-expression -> AST
(define (parse sexp)
  (define (parse-op opsexp)
    (let (
          [operador (case (first opsexp)

                      [(+) +]
                      [(-) -]
                      [(*) *]
                      [(/) /]
                      [(modulo) modulo]
                      [(expt) expt]
                      [(not) not])])
      (op operador (map parse (rest opsexp)))
      )
  )
  (define (parse-op-bool opsexp)
    (let (
          [operador (case (first opsexp)
                      [(and) 'and]
                      [(or) 'or])])
      (op-bool operador (parse (second opsexp)) (parse (third opsexp)))
      )
  )
  (cond
    [(symbol? sexp)
        (case sexp
          [(T) (bool #t)]
          [(F) (bool #f)]
          [(lempty) (lempty)]
          [else (id sexp)]
          )]
    [(number? sexp) (num sexp)]
    [(list? sexp) (case (first sexp)
          [(+ - * / modulo expt not) (parse-op sexp)]
          [(or and) (parse-op-bool sexp)]
          [(with) (with (map appBinding (second sexp)) (parse (third sexp)))]
          [(with*) (with* (map appBinding (second sexp)) (parse (third sexp)))]
          [(lcons) (lcons (parse (second sexp)) (parse (third sexp)))]
          [(lcar) (lcar (parse (second sexp)))]
          [(lcdr) (lcdr (parse (second sexp)))]
          [(fun) (fun (second sexp) (funBody (third sexp)))]
          [(app)
           (if (equal? 'fun (first (second sexp)))
               (app (parse (second sexp)) (map parse (rest (rest sexp))))
               (error "No hay una funcion")
           )
          ]
     )]
    )
  )


;; Funcion auxiliar de parse que transforma a binding la pareja pasada
(define (appBinding pareja)
  (binding (first pareja) (parse (second pareja))))

;; Funcion auxiliar de parse que realiza el parse del cuerpo de la funcion
(define (funBody body)
  (if (empty? (rest body))
              (parse (first body))
              (parse body)))



;; ******************************************************************

;; 2. (3 pts). Funcion (desugar s-fwael-expr) que elimina el azucar sintactica de la expresion FWAEL
;; dada; la cual se encuentra en forma de Arbol de Sintaxis Abstracta, y devuelve un nuevo arbol FWAEL
;; sin azucar sintactica.
;; * Precondiciones: Una expresion FWAEL en su representacion como AST.
;; * Postcondiciones: Una expresión FWAEL sin azucar sintáctica representada en un AST.
;;   desugar: AST -> AST

(define (desugar s-fwael-expr)
  (cond
    [(id? s-fwael-expr) s-fwael-expr]
    [(num? s-fwael-expr) s-fwael-expr]
    [(bool? s-fwael-expr) s-fwael-expr]
    [(op? s-fwael-expr)
     (op (op-f s-fwael-expr) (desugar-list (op-args s-fwael-expr)))]
                          ;;[(eq? + (op-f s-fwael-expr)) (op + (desugar-list (op-args s-fwael-expr)))]
    [(op-bool? s-fwael-expr)
     (op-bool (op-bool-f s-fwael-expr) (desugar-list (op-bool-larg s-fwael-expr)) (desugar-list (op-bool-rarg s-fwael-expr)))]
    [(with? s-fwael-expr)
     (app (fun (list (binding-id (first (with-bindings s-fwael-expr)))) (desugar (with-body s-fwael-expr)))
          (list (desugar (binding-value (first (with-bindings s-fwael-expr))))))]
    [(with*? s-fwael-expr) (desugar-mw s-fwael-expr)]
    [(fun? s-fwael-expr) (desugar-fun s-fwael-expr)]
    [(lempty? s-fwael-expr) s-fwael-expr]
    [(lcons? s-fwael-expr) (lcons (desugar (lcons-l s-fwael-expr))
                                  (desugar (lcons-r s-fwael-expr)))]
    [(lcar? s-fwael-expr) (lcar (desugar (lcar-lst s-fwael-expr)))]
    [(lcdr? s-fwael-expr) (lcdr (desugar (lcdr-lst s-fwael-expr)))]
    [(app? s-fwael-expr)
     (desugar-app s-fwael-expr)]
  )
)

;; EJEMPLO DE OP
;; (op + (list (num 3) (num 3)))
;; EJEMPLO DE WITH*
;; (with* (list (binding 'x (num 4)) (binding 'y (num 3))) (op + (list (id 'x) (id 'y))))
;; EJEMPLO DE FUN
;; (fun (list 'x 'y) (op + (list (id 'x) (id 'y))))
;; EJEMPLO DE APP
;; (app (fun (list 'x 'y) (op + (list (id 'x) (id 'y)))) (list (num 2) (num 3)))

;; Funcion auxiliar que dado una lista de AST realiza desugar de cada elemento 
(define (desugar-list ls)
  (if (empty? ls)
      '()
      (cons (desugar (first ls)) (desugar-list (cdr ls)))
  )
)

;; Funcion auxiliar que dado un with* realiza su desugar 
(define (desugar-mw mw)
  (if (eq? 1 (length (with*-bindings mw)))
      (with (desugar-bs (with*-bindings mw)) (desugar (with*-body mw)))
      (with (desugar-bs (list (first (with*-bindings mw))))
            (desugar-mw (with* (cdr (with*-bindings mw)) (desugar (with*-body mw)))))
  )
)

;; Funcion auxiliar que dado una lista de binding realiza el desugar de los values 
(define (desugar-bs bs)
  (if (eq? 0 (length bs))
      '()
      (cons (binding (binding-id (first bs)) (desugar (binding-value (first bs)))) (desugar-bs (cdr bs)))
  )
)

;; Funcion auxiliar que dado una fun realiza su desugar
(define (desugar-fun func)
  (if (eq? 1 (length (fun-params func)))
      (fun (fun-params func) (desugar (fun-body func)))
      (fun (list (first (fun-params func)))
           (desugar-fun (fun (cdr (fun-params func)) (fun-body func))))
  )
)

;; Funcion auxiliar que dado una app realiza su desugar
(define (desugar-app ap)
  (if (eq? 1 (length (app-args ap)))
      (app (desugar-fun (app-fun ap)) (desugar-list (app-args ap)))
      (app (fun (list (first (fun-params (app-fun ap)))) (desugar-app (app (fun (cdr (fun-params (app-fun ap)))
                                                                                (fun-body (app-fun ap)))
                                                                           (cdr (app-args ap)))))
           (list (desugar (first (app-args ap)))))
  )
)



;; ******************************************************************

;; 3. (1 pts). Funcion que realiza la sustitucion del parametro formal sub-id de la funcion de
;; la que la expresion FWAEL sin azúcar sintactica que forma parte de su cuerpo, por el
;; parametro real que debería encontrarse en el ambiente (Environment) env. En caso de no
;; encontrar el simbolo sub-id que representa el parametro formal en el ambiente env,
;; envia un error.
;; * Precondiciones: Una expresion FWAEL sin azucar sintactica representada en un AST y
;;   un ambiente.
;; * Postcondiciones: Una expresion FWAEL sin azucar sintactica representada como AST
;;   donde se ha sustituido el parametro formal de la expresion del cuerpo de la funcion
;;   dada por el parámetro real que deberia estar en el ambiente. En caso de que el
;;   parametro formal no se encuentre en el ambiente, envia un error.
;; subst: AST, Environment -> AST





;; ******************************************************************

;; 4. (3 pts). Funcion que evalua una expresion FWAEL sin azucar sintactica
;; dada bajo el ambiente (Environment) env dado. El resultado de la evaluacion debe ser un valor FWAEL
;; (FWAEL-Value) y no un valor de Racket. Para evaluar expresiones fun es necesario utilizar la funcion subst.
;; * Precondiciones: una expresion FWAEL en su representacion como Arbol de Sintaxis Abstracta (AST), sin azucar
;;   sintactica y un ambiente de ejecucion.
;; * Postcondiciones: un valor FWAEL al que se evalua la expresion en el ambiente dado.
;; interp: AST, Environment -> FWAEL-Value
(define (interp fwael-expr env)
  (cond
    ;;[(id? fwael-expr) #f]
    [(id? fwael-expr) (error "error: Variable libre")]
    [(num? fwael-expr) (num-n fwael-expr)]
    [(bool? fwael-expr) (bool-b fwael-expr)]
    [(op? fwael-expr) (cond
                      [(eq? + (op-f fwael-expr)) (operSum (map interp (op-args fwael-expr)))]
                      [(eq? - (op-f fwael-expr)) (operRes (map interp (op-args fwael-expr)))]
                      [(eq? * (op-f fwael-expr)) (operProd (map interp (op-args fwael-expr)))]
                      [(eq? / (op-f fwael-expr)) (operDiv (map interp (op-args fwael-expr)))]
                      [(eq? modulo (op-f fwael-expr)) (operMod (map interp (op-args fwael-expr)))]
                      [(eq? expt (op-f fwael-expr)) (operExpt (map interp (op-args fwael-expr)))]
                      [(eq? not (op-f fwael-expr)) (operNot (map interp (op-args fwael-expr)))])]
    [(with? fwael-expr) (let* (
                             [bdgs (with-bindings fwael-expr)]
                             [cuerpo (with-body fwael-expr)])
                             (interp (evalWith cuerpo bdgs)))]
    [(with*? fwael-expr) (let* (
                             [bdgs (with*-bindings fwael-expr)]
                             [cuerpo (with*-body fwael-expr)]
                             [newBdgs (interpBdgs bdgs bdgs)])
                             (interp (evalWith cuerpo newBdgs)))]
    [(fun? fwael-expr) fwael-expr]
    [(app? fwael-expr) (let* (
        [apped-fun (app-fun fwael-expr)]
        [args (app-args fwael-expr)]
        [fun-arg (first (fun-params apped-fun))]
        [env (aSub fun-arg (first args) env)])
        (interp (subst apped-fun fun-arg env) env)
      )]
    [else ("Error de sintaxis")]
  )
)


;; Funcion auxuliar de interp que realiza la operacion de suma sobre una lista de numeros.
;; * Precondiciones: una lista de numeros de tamano mayor a 1.
;; * Postcondiciones: la suma de los numeros de la lista, si la lista es de tamano menor a 1 se lanza
;;   un error.
(define (operSum l)
  (if (> (length l) 1)
      (foldl + 0 l)
      (error "error: + requiere mas de un parametro")))

;; Funcion auxuliar de interp que realiza la operacion de resta sobre una lista de numeros.
;; * Precondiciones: una lista de numeros de tamano mayor a 1.
;; * Postcondiciones: la resta de los numeros de la lista, si la lista es de tamano menor a 1 se lanza
;;   un error.
(define (operRes l)
  (if (> (length l) 1)
      (foldr - 0 l)
      (error "error: - requiere mas de un parametro")))

;; Funcion auxuliar de interp que realiza la operacion de producto sobre una lista de numeros.
;; * Precondiciones: una lista de numeros de tamano mayor a 1.
;; * Postcondiciones: el producto de los numeros de la lista, si la lista es de tamano menor a 1 se lanza
;;   un error.
(define (operProd l)
  (if (> (length l) 1)
      (foldl * 1 l)
      (error "error: * requiere mas de un parametro")))

;; Funcion auxuliar de interp que realiza la operacion de division sobre una lista de numeros.
;; * Precondiciones: una lista de numeros de tamano 2.
;; * Postcondiciones: la division de los numeros de la lista, si la lista es de tamano distinto de 2 se lanza
;;   un error.
(define (operDiv l)
  (if (eq? (length l) 2)
      (foldr / 1 l)
      (error "error: / requiere dos parametros")))

;; Funcion auxuliar de interp que realiza la operacion de modulo sobre una lista de numeros
;; * Precondiciones: una lista de numeros de tamano 2.
;; * Postcondiciones: el modulo de los numeros de la lista, si la lista es de tamano distinto de 2 se lanza
;;   un error.
(define (operMod l)
  (if (eq? (length l) 2)
      (modulo (first l) (second l))
      (error "error: Modulo requiere dos parametros")))

;; Funcion auxuliar de interp que realiza la operacion de expt sobre una lista de numeros
;; * Precondiciones: una lista de numeros de tamano 2.
;; * Postcondiciones: el expt de los numeros de la lista, si la lista es de tamano distinto de 2 se lanza
;;   un error.
(define (operExpt l)
  (if (eq? (length l) 2)
      (expt (first l) (second l))
      (error "error: Expt requiere dos parametros")))

;; Funcion auxuliar de interp que realiza la operacion de not sobre una lista de booleanos
;; * Precondiciones: una lista de booleanos de tamano 1.
;; * Postcondiciones: la negacion del booleano de la lista, si la lista es de tamano mayor a 1 se lanza
;;   un error.
(define (operNot l)
  (if (eq? (length l) 1)
      (not (first l))
      (error "error: Not requiere un parametro")))

;; Funcion auxiliar de interp que evalua el with
(define (evalWith expr bdgs)
  (if (eq? 0 (length bdgs))
      expr
      (evalWith (subst expr (binding-id (car bdgs)) (binding-value (car bdgs))) (rest bdgs))))

;; Funcion auxiliar de interp que sustituye los ids en el resto de la lista de identificadores
(define (interpBdgs bdgs bdgsAux)
  (if (eq? 1 (length bdgsAux))
      bdgs
      (cons (car bdgs) (interpBdgs (interpBdg (car bdgs) (cdr bdgs)) (rest bdgsAux)))))

;; Funcion auxiliar de interpBdgs que sustituye el binding dado en la lista de indentificadores
(define (interpBdg bdg body)
  (if(eq? 0 (length body))
     body
     (cons (substBdg bdg (car body)) (interpBdg bdg (rest body)))))

;; Funcion auxiliar de interpBdg que hace el subst de un binding a otro binding
(define (substBdg bdg1 bdg2)
  (binding (binding-id bdg2) (subst (binding-value bdg2) (binding-id bdg1) (binding-value bdg1))))

;; Funcion auxiliar de interp que evalua la funcion dados los parametros y los argumentos
(define (evaluaFunc params oper argus)
  (cond
    [(not (eq? (length params) (length argus))) (error "error: El numero de parametros es inadecuado")]
    [else (interp (subst-varios oper params argus))]
   )
)

;; Funcion auxiliar de evaluaFunc para sustituir los valores de los parametros y evaluar la funcion
(define (subst-varios oper ids valores)
  (if (eq? 0 (length ids))
      oper
      (subst-varios (subst oper (first ids) (first valores)) (rest ids) (rest valores))))
