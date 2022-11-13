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
     (op-bool (op-bool-f s-fwael-expr) (desugar (op-bool-larg s-fwael-expr)) (desugar (op-bool-rarg s-fwael-expr)))]
    [(with? s-fwael-expr)
     (app (fun (list (binding-id (first (with-bindings s-fwael-expr)))) (desugar (with-body s-fwael-expr)))
          (list (desugar (binding-value (first (with-bindings s-fwael-expr))))))]
    [(with*? s-fwael-expr) (desugar (desugar-mw s-fwael-expr))]
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
(define (subst fwael-expr sub-id env)
    (define (find-inenv env)
        (if (mtSub? env)
            (error "Error" sub-id "es una variable libre")
            (if (eq? (aSub-name env) sub-id)
                (aSub-value env)
                (find-inenv (aSub-bSub env))
            )
        )
    )
    (cond
        [(id? fwael-expr) (if (eq? sub-id (id-i fwael-expr))
                              (find-inenv env)
                              fwael-expr)]
        [(num? fwael-expr) fwael-expr]
        [(bool? fwael-expr) fwael-expr]
        [(op? fwael-expr)
         (op (op-f fwael-expr) (subst-list (op-args fwael-expr) sub-id env))]
        [(op-bool? fwael-expr)
         (op-bool (op-bool-f fwael-expr) (subst (op-bool-larg fwael-expr) sub-id env)
                                         (subst (op-bool-rarg fwael-expr) sub-id env))]
        [(fun? fwael-expr) (subst-fun fwael-expr sub-id env)]
        [(app? fwael-expr) (subst-app fwael-expr sub-id env)]
        [(lcons? fwael-expr) (lcons (subst (lcons-l fwael-expr) sub-id env) (subst (lcons-r fwael-expr) sub-id env))]
        [(lcar? fwael-expr) (lcar (subst (lcar-lst fwael-expr) sub-id env))]
        [(lcdr? fwael-expr) (lcdr (subst (lcdr-lst fwael-expr) sub-id env))]
        [else fwael-expr]
    )
)

;; Funcion auxiliar que realiza subs en una lista de elementos AST sin azucar sintactica
(define (subst-list ls i env)
  (if (empty? ls)
      '()
      (cons (subst (first ls) i env) (subst-list (cdr ls) i env))
  )
)

;; Funcion auxiliar que hace subst de una fun
(define (subst-fun f i env)
  (if (eq? i (first (fun-params f)))
      (subst (fun-body f) i env)
      (fun (fun-params f) (subst (fun-body f) i env))
  )
)

;; Funcion auxiliar que hace subst de una app
(define (subst-app ap i env)
  (if (eq? i (first (fun-params (app-fun ap))))
      (subst (app-fun ap) i env)
      (subst (subst (app-fun ap) (first (fun-params (app-fun ap))) (aSub (first (fun-params (app-fun ap)))
                                                                  (first (app-args ap))
                                                                  (aSub (first (fun-params (app-fun ap))) (first (app-args ap)) env))) i env)
  )
)


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
    [(id? fwael-expr) (error "error: Variable libre")]
    [(num? fwael-expr) (numV (num-n fwael-expr))]
    [(lempty? fwael-expr) (listV empty)]
    [(bool? fwael-expr) (boolV (bool-b fwael-expr))]
    [(op? fwael-expr) (cond
                      [(eq? + (op-f fwael-expr)) (interp (parse (operSum (interpOp (op-args fwael-expr) env))) env)]
                      [(eq? - (op-f fwael-expr)) (interp (parse (operRes (interpOp (op-args fwael-expr) env))) env)]
                      [(eq? * (op-f fwael-expr)) (interp (parse (operProd (interpOp (op-args fwael-expr) env))) env)]
                      [(eq? / (op-f fwael-expr)) (interp (parse (operDiv (interpOp (op-args fwael-expr) env))) env)]
                      [(eq? modulo (op-f fwael-expr)) (interp (parse (operMod (interpOp (op-args fwael-expr) env))) env)]
                      [(eq? expt (op-f fwael-expr)) (interp (parse (operExpt (interpOp (op-args fwael-expr) env))) env)]
                      [(eq? not (op-f fwael-expr)) (interp (bool (operNot (interpOp (op-args fwael-expr) env))) env)])]
    [(op-bool? fwael-expr) (cond
                            [(eq? 'or (op-bool-f fwael-expr)) (interp (bool (or (interp-val (op-bool-larg fwael-expr)) (interp-val (op-bool-rarg fwael-expr)))) env)]
                            [(eq? 'and (op-bool-f fwael-expr)) (interp (bool (and (interp-val (op-bool-larg fwael-expr)) (interp-val (op-bool-rarg fwael-expr)))) env)])]
    [(fun? fwael-expr) (closureV (fun-params fwael-expr) (fun-body fwael-expr) env)]
    [(lcons? fwael-expr) (let (
                               [l-expr (interp (lcons-l fwael-expr) env)]
                               [r-expr (interp (lcons-r fwael-expr) env)])
                           (listV (list l-expr r-expr)))]
    [(lcar? fwael-expr) (interp (lcons-l (lcar-lst fwael-expr)) env)]
    [(lcdr? fwael-expr) (interp (lcons-r (lcdr-lst fwael-expr)) env)]
    [(app? fwael-expr) (let* (
        [apped-fun (app-fun fwael-expr)]
        [args (app-args fwael-expr)]
        [fun-arg (first (fun-params apped-fun))]
        [env (aSub fun-arg (first args) env)])
        (interp (substFunc apped-fun env) env)
      )]
    [else ("Error de sintaxis")]
  )
)

;; Funcion auxiliar de interp que convierte a valores de Racket los valores de tipo FWAEL.
;; * Precondiciones: lista de valores de tipo FWAEL
;; * Postcondiciones: lista de valores de Racket 
(define (interpOp args env)
  (if (empty? args)
      '()
      (cons (interp-val (car args)) (interpOp (cdr args) env))))

;; Funcion auxiliar de interp que convierte un a valor de Racket el valor de tipo FWAEL.
;; * Precondiciones: valor de tipo FWAEL
;; * Postcondciones: valor transformado a Racket
(define (interp-val fwael-ast)
  (cond
    [(id? fwael-ast) (error "error: Variable libre")]
    [(num? fwael-ast) (num-n fwael-ast)]
    [(bool? fwael-ast) (bool-b fwael-ast)]
    [(op? fwael-ast) (cond
                      [(eq? + (op-f fwael-ast)) (operSum (map interp-val (op-args fwael-ast)))]
                      [(eq? - (op-f fwael-ast)) (operRes (map interp-val (op-args fwael-ast)))]
                      [(eq? * (op-f fwael-ast)) (operProd (map interp-val (op-args fwael-ast)))]
                      [(eq? / (op-f fwael-ast)) (operDiv (map interp-val (op-args fwael-ast)))]
                      [(eq? modulo (op-f fwael-ast)) (operMod (map interp-val (op-args fwael-ast)))]
                      [(eq? expt (op-f fwael-ast)) (operExpt (map interp-val (op-args fwael-ast)))]
                      [(eq? not (op-f fwael-ast)) (operNot (map interp-val (op-args fwael-ast)))])]
    [(op-bool? fwael-ast) (cond
                           [(eq? 'or (op-bool-f fwael-ast)) (or (interp-val (op-bool-larg fwael-ast)) (interp-val (op-bool-rarg fwael-ast)))]
                           [(eq? 'and (op-bool-f fwael-ast)) (and (interp-val (op-bool-larg fwael-ast)) (interp-val (op-bool-rarg fwael-ast)))])]
    ))

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
 
;; Funcion auxiliar de  que verifica si un id esta en el ambiente
(define (estaEnEnv id env)
  (if (mtSub? env)
      #f
      (if (eq? id (aSub-name env))
          #t
          (estaEnEnv id (aSub-bSub env)))))

;; Funcion auxiliar de interp para poder aplicar subst dependiendo de los ids que este en el ambiente
(define (substFunc func env)
  (let* (
         [body (fun-body func)]
         [variables (varFunc body)]
         [ids (filterIds variables)])
    (substFuncAux func ids env)))
  
;; Funcion auxiliar de interp que permite aplicar subst a cada funcion anidada
(define (substFuncAux func ids env)
  (if (empty? ids)
      func
      (if (eq? (estaEnEnv (car ids) env) #t)
          (substFuncAux (substAppFunc func (car ids) env) (cdr ids) env)
          (substFuncAux func (cdr ids) env)))) 
       
;; Funcion auxiliar de interp que aplica el subst a la funcion con un id dado
(define (substAppFunc func id env)
  (subst func id env))

;; Funcion auxiliar de interp que encuentra las variables del cuerpo de la funcion
(define (varFunc func-body)
  (cond
    [(id? func-body) (id-i func-body)]
    [(num? func-body) (num-n func-body)]
    [(lempty? func-body) func-body]
    [(bool? func-body) (bool-b func-body)]
    [(op? func-body) (map varFunc (op-args func-body))]
    [(op-bool? func-body) (list  (varFunc (op-bool-larg func-body)) (varFunc (op-bool-rarg func-body)))]
    [(lcons? func-body) (list (varFunc (lcons-l func-body)) (varFunc (lcons-r func-body)))]
    [(lcar? func-body) (varFunc (lcar-lst func-body))]
    [(lcdr? func-body) (varFunc (lcdr-lst func-body))]
    [(fun? func-body) (varFunc (fun-body func-body))]
    [(app? func-body) (varFunc (app-fun func-body))]
    )
)
    
;; Funcion auxiliar de varFunc que filtra los ids
(define (filterIds ids)
  (if (empty? ids)
      '()
      (cond
        [(symbol? (car ids)) (cons (car ids) (filterIds (cdr ids)))]
        [(number? (car ids)) (filterIds (cdr ids))]
        [(boolean? (car ids)) (filterIds (cdr ids))]
        [(list? (car ids)) (filterIds (car ids))]
        [else (filterIds (cdr ids))])))

;; ******************************************************************

;; 5. (1pto Extra). Funcion que calcule el perimetro de una elipse en el lenguaje FWAEL.
;; * Precondiciones: dos numeros que representan respectivamente el eje semi-mayor y el eje semi-menor de la
;;   elipse.
;; * Postcondiciones: el perimetro encerrado por la elipse con ejes semi-mayor y semi-menor dados.
;; perimelipse: AST-num, AST-num -> FWAEL-Value
(define (perimelipse semi-mayor semi-menor)
  (interp (desugar (app (fun '(x y) (op * (list (op * (list (num 2) (num 3.1415))) (op operPerim (list (id 'x) (id 'y)))))) (list semi-mayor semi-menor))) (mtSub)))
 
(define (operPerim semi-mayor semi-menor)
  (sqrt (/ (+ (expt semi-mayor 2) (expt semi-menor 2)) 2)))
