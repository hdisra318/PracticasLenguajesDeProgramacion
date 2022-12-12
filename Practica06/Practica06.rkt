#|
Practica 06
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
  [branch (test AST?) (then AST?) (else AST?)]
  [multi-branch (conds (listof branch-cond?)) (else AST?)]
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
  [branch-cond (test AST?) (then AST?)]
)

;; Tipo de dato para los ambientes (Environment)
(define-type Environment
  [mtSub]
  [aSub (name symbol?) (value AST?) (bSub Environment?)])

;; Tipo de dato para los valores FWAEL
(define-type TCFWAEL-Value
  [numV (n number?)]
  [boolV (b boolean?)]
  [listV (l (listof TCFWAEL-Value?))]
  [closureV (param (listof symbol?)) (body AST?) (env Environment?)])


;; ******************************************************************

;; 1. (1.5 pts). Defina la funcion (parse sexp), la cual recibe una expresion simbolica (symbolic expression,
;; s-expression); esto es, la expresion puede ser un numero, un simbolo o una lista de expresiones simboli-
;; cas. (parse sexp) debe construir un Arbol de Sintaxis Abstracta (Abstract Syntax Tree - AST) a partir de la
;; s-expression si es una expresion valida en el lenguaje TCFWAEL. En otro caso, debe arrojar un error.
;; * Precondiciones: una expresion simbolica (symbolic expression; s-expression).
;; * Postcondiciones: si la s-expression de entrada satisface la gramática del lenguaje TCFWAEL;
;; devuelve un árbol de sintaxis abstracta AST, de lo contrario, enviar un error.
;; parse: s-expression -> AST
(define (parse sexp)
  (define (parse-op opsexp)
    (let (
          [operador (case (first opsexp)
                      [(+) +]
                      [(-) -]
                      [(*) *]
                      [(/) /]
                      [(=) =]
                      [(<) <]
                      [(>) >]
                      [(<=) <=]
                      [(>=) >=]
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
          [(+ - * / = < > >= <= modulo expt not) (parse-op sexp)]
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
          [(if) (branch (parse (second sexp))
                        (parse (third sexp))
                        (parse (fourth sexp)))]
          [(cond) (multi-branch
                   (map (lambda (cond)
                        (branch-cond (parse (first cond))
                            (parse (second cond))))
                        (second sexp))
                   (parse (third sexp)))]
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

;; 2. (1.5 pts). Funcion que elimina el azucar sintactica de la expresion TCFWAEL
;; dada; la cual se encuentra en forma de Arbol de Sintaxis Abstracta, y devuelve
;; un nuevo arbol TCFWAEL sin azucar sintactica.
;; * Precondiciones: Una expresion TCFWAEL en su representacion como AST.
;; * Postcondiciones: Una expresión TCFWAEL sin azucar sintactica representada en un AST.
;; desugar: AST -> AST
(define (desugar s-tcfwael-expr)
  (cond
    [(id? s-tcfwael-expr) s-tcfwael-expr]
    [(num? s-tcfwael-expr) s-tcfwael-expr]
    [(bool? s-tcfwael-expr) s-tcfwael-expr]
    [(op? s-tcfwael-expr)
     (op (op-f s-tcfwael-expr) (desugar-list (op-args s-tcfwael-expr)))]
    [(op-bool? s-tcfwael-expr)
     (op-bool (op-bool-f s-tcfwael-expr) (desugar (op-bool-larg s-tcfwael-expr)) (desugar (op-bool-rarg s-tcfwael-expr)))]
    [(with? s-tcfwael-expr)
     (app (fun (list (binding-id (first (with-bindings s-tcfwael-expr)))) (desugar (with-body s-tcfwael-expr)))
          (list (desugar (binding-value (first (with-bindings s-tcfwael-expr))))))]
    [(with*? s-tcfwael-expr) (desugar (desugar-mw s-tcfwael-expr))]
    [(fun? s-tcfwael-expr) (desugar-fun s-tcfwael-expr)]
    [(lempty? s-tcfwael-expr) s-tcfwael-expr]
    [(lcons? s-tcfwael-expr) (lcons (desugar (lcons-l s-tcfwael-expr))
                                  (desugar (lcons-r s-tcfwael-expr)))]
    [(lcar? s-tcfwael-expr) (lcar (desugar (lcar-lst s-tcfwael-expr)))]
    [(lcdr? s-tcfwael-expr) (lcdr (desugar (lcdr-lst s-tcfwael-expr)))]
    [(app? s-tcfwael-expr)
     (desugar-app s-tcfwael-expr)]
    [(branch?  s-tcfwael-expr) s-tcfwael-expr]
    [(multi-branch? s-tcfwael-expr) (desugar-mb s-tcfwael-expr)] 
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

;; Funcion auxiliar que dado un multi-branch realiza su desugar 
(define (desugar-mb mb)
  (if (= 1 (length (multi-branch-conds mb)))
      (branch (branch-cond-test (first (multi-branch-conds mb)))
              (branch-cond-then (first (multi-branch-conds mb)))
              (multi-branch-else mb))
      (branch (branch-cond-test (first (multi-branch-conds mb)))
              (branch-cond-then (first (multi-branch-conds mb)))
              (desugar-mb (multi-branch (cdr (multi-branch-conds mb)) (multi-branch-else mb)))))
)


;; ******************************************************************

;; 3. (1 pts). Funcion que realiza la sustitucion del parametro formal sub-id de la funcion
;; de la que la expresión TCFWAEL sin azucar sintactica que forma parte de su cuerpo, por el
;; parametro real que deberia encontrarse en el ambiente (Environment) env.
;; En caso de no encontrar el simbolo sub-id que representa el parametro formal en el
;; ambiente env, debe enviar un error.
;; * Precondiciones: Una expresion TCFWAEL sin azucar sintactica representada en
;;   un AST y un ambiente.
;; * Postcondiciones: Una expresion TCFWAEL sin azucar sintactica representada como AST
;;   donde se ha sustituido el parametro formal de la expresion del cuerpo de la funcion
;    dada por el parametro real que deberia estar en el ambiente.
;    En caso de que el parametro formal no se encuentre en el ambiente, debe enviar un error.
;; subst: AST, Environment -> AST
(define (subst tcfwael-expr sub-id env)
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
        [(id? tcfwael-expr) (if (eq? sub-id (id-i tcfwael-expr))
                              (find-inenv env)
                              tcfwael-expr)]
        [(num? tcfwael-expr) tcfwael-expr]
        [(bool? tcfwael-expr) tcfwael-expr]
        [(op? tcfwael-expr)
         (op (op-f tcfwael-expr) (subst-list (op-args tcfwael-expr) sub-id env))]
        [(op-bool? tcfwael-expr)
         (op-bool (op-bool-f tcfwael-expr) (subst (op-bool-larg tcfwael-expr) sub-id env)
                                         (subst (op-bool-rarg tcfwael-expr) sub-id env))]
        [(fun? tcfwael-expr) (subst-fun tcfwael-expr sub-id env)]
        [(app? tcfwael-expr) (subst-app tcfwael-expr sub-id env)]
        [(lcons? tcfwael-expr) (lcons (subst (lcons-l tcfwael-expr) sub-id env) (subst (lcons-r tcfwael-expr) sub-id env))]
        [(lcar? tcfwael-expr) (lcar (subst (lcar-lst tcfwael-expr) sub-id env))]
        [(lcdr? tcfwael-expr) (lcdr (subst (lcdr-lst tcfwael-expr) sub-id env))]
        [else tcfwael-expr]
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

;; 4. (3 pts). Funcion (check-type tcfwael-expr) que indica el tipo de una expresion TCFWAEL sin azucar
;; sintáctica ni identificadores (variables). Note que los tipos a los que se reduce una expresion son fijos por cada
;; tipo de operacion TCFWAEL; sin importar el valor de sus parametros o argumentos (en el siguiente ejercicio, con
;; interp, deberá cuidar los tipos de estos parámetros o argumentos).
;; * Precondiciones: una expresion TCFWAEL en su representacion cómo Arbol de Sintaxis Abstracta (AST).
;; * Postcondiciones: el tipo TCFWAEL-Value al que evaluara la expresión TCFWAEL conforme a las condiciones
;; descritas en este ejercicio.
;; check-type: AST -> TCFWAEL-Value
(define (check-type tcfwael-expr)
  (cond
    [(num? tcfwael-expr) numV]
    [(bool? tcfwael-expr) boolV]
    [(op? tcfwael-expr)
     (cond
       [(eq? + (op-f tcfwael-expr)) numV]
       [(eq? - (op-f tcfwael-expr)) numV]
       [(eq? * (op-f tcfwael-expr)) numV]
       [(eq? / (op-f tcfwael-expr)) numV]
       [(eq? = (op-f tcfwael-expr)) boolV]
       [(eq? < (op-f tcfwael-expr)) boolV]
       [(eq? > (op-f tcfwael-expr)) boolV]
       [(eq? <= (op-f tcfwael-expr)) boolV]
       [(eq? >= (op-f tcfwael-expr)) boolV]
       [(eq? modulo (op-f tcfwael-expr)) numV]
       [(eq? expt (op-f tcfwael-expr)) numV]
       [(eq? not (op-f tcfwael-expr)) boolV]
     )]
    [(op-bool? tcfwael-expr)
     (cond
       [(eq? 'and (op-bool-f tcfwael-expr)) boolV]
       [(eq? 'or (op-bool-f tcfwael-expr)) boolV]
     )
    ]
    [(branch? tcfwael-expr) (check-type-if tcfwael-expr)]
    [(fun? tcfwael-expr) (check-type (fun-body tcfwael-expr))]
    [(lcons? tcfwael-expr) listV]
    [(lempty? tcfwael-expr) listV]
    [(lcar? tcfwael-expr) (check-type (lcar-lst tcfwael-expr))]
    [(lcdr? tcfwael-expr) listV]
    [(app? tcfwael-expr) (check-type (fun-body (app-fun tcfwael-expr)))]
  )
)

(define (check-type-if c)
  (if (and #t (boolV-b (interp (branch-test c) [mtSub])))
      (check-type (branch-then c))
      (check-type (branch-else c))
  )
)

;; ******************************************************************

;; 5. (3 pts). Funcion que evalúa una expresion TCFWAEL sin azucar sintactica dada bajo
;; el ambiente (Environment) env dado. El resultado de la evaluacion debe ser un valor TCFWAEL.
;; (TCFWAEL-Value) y no un valor de Racket.
;; * Precondiciones: una expresión TCFWAEL en su representacion como Arbol de Sintaxis Abstracta (AST)
;; y un ambiente de ejecucion.
;; * Postcondiciones: un valor TCFWAEL al que se evalua la expresión TCFWAEL en el ambiente
;; dado conforme a las condiciones descritas.
;; interp: AST, Environment -> TCFWAEL-Value
(define (interp tcfwael-expr env)
  (cond
    [(id? tcfwael-expr) (error "error: Variable libre")]
    [(num? tcfwael-expr) (numV (num-n tcfwael-expr))]
    [(lempty? tcfwael-expr) (listV empty)]
    [(bool? tcfwael-expr) (boolV (bool-b tcfwael-expr))]
    [(op? tcfwael-expr) (cond
                      [(eq? + (op-f tcfwael-expr)) (if (eq? #t (argsNumV (op-args tcfwael-expr))) (interp (parse (operSum (interpOp (op-args tcfwael-expr) env))) env)
                                                       (error "error: Hay algun argumento que no es de tipo numV"))]
                      [(eq? - (op-f tcfwael-expr)) (if (eq? #t (argsNumV (op-args tcfwael-expr))) (interp (parse (operRes (interpOp (op-args tcfwael-expr) env))) env)
                                                       (error "error: Hay algun argumento que no es de tipo numV"))]
                      [(eq? * (op-f tcfwael-expr)) (if (eq? #t (argsNumV (op-args tcfwael-expr))) (interp (parse (operProd (interpOp (op-args tcfwael-expr) env))) env)
                                                       (error "error: Hay algun argumento que no es de tipo numV"))]
                      [(eq? / (op-f tcfwael-expr)) (if (eq? #t (argsNumV (op-args tcfwael-expr))) (interp (parse (operDiv (interpOp (op-args tcfwael-expr) env))) env)
                                                       (error "error: Hay algun argumento que no es de tipo numV"))]
                      [(eq? = (op-f tcfwael-expr)) (if (eq? #t (argsNumV (op-args tcfwael-expr))) (interp (bool (= (num-n (first (op-args tcfwael-expr))) (num-n (second (op-args tcfwael-expr))))) env)
                                                       (error "error: Hay algun argumento que no es de tipo numV"))]
                      [(eq? < (op-f tcfwael-expr)) (if (eq? #t (argsNumV (op-args tcfwael-expr))) (interp (bool (< (num-n (first (op-args tcfwael-expr))) (num-n (second (op-args tcfwael-expr))))) env)
                                                       (error "error: Hay algun argumento que no es de tipo numV"))]
                      [(eq? > (op-f tcfwael-expr)) (if (eq? #t (argsNumV (op-args tcfwael-expr))) (interp (bool (> (num-n (first (op-args tcfwael-expr))) (num-n (second (op-args tcfwael-expr))))) env)
                                                       (error "error: Hay algun argumento que no es de tipo numV"))]
                      [(eq? <= (op-f tcfwael-expr)) (if (eq? #t (argsNumV (op-args tcfwael-expr))) (interp (bool (<= (num-n (first (op-args tcfwael-expr))) (num-n (second (op-args tcfwael-expr))))) env)
                                                        (error "error: Hay algun argumento que no es de tipo numV"))]
                      [(eq? >= (op-f tcfwael-expr)) (if (eq? #t (argsNumV (op-args tcfwael-expr))) (interp (bool (>= (num-n (first (op-args tcfwael-expr))) (num-n (second (op-args tcfwael-expr))))) env)
                                                        (error "error: Hay algun argumento que no es de tipo numV"))]
                      [(eq? modulo (op-f tcfwael-expr)) (if (eq? #t (argsNumV (op-args tcfwael-expr))) (interp (parse (operMod (interpOp (op-args tcfwael-expr) env))) env)
                                                            (error "error: Hay algun argumento que no es de tipo numV"))]
                      [(eq? expt (op-f tcfwael-expr)) (if (eq? #t (argsNumV (op-args tcfwael-expr))) (interp (parse (operExpt (interpOp (op-args tcfwael-expr) env))) env)
                                                          (error "error: Hay algun argumento que no es de tipo numV"))]
                      [(eq? not (op-f tcfwael-expr)) (if (eq? #t (argsBoolV (op-args tcfwael-expr))) (interp (bool (operNot (interpOp (op-args tcfwael-expr) env))) env)
                                                         (error "error: Hay algun argumento que no es de tipo boolV"))])]
    [(op-bool? tcfwael-expr) (cond
                            [(eq? 'or (op-bool-f tcfwael-expr)) (if (eq? #t (argsBoolV (list (op-bool-larg tcfwael-expr) (op-bool-rarg tcfwael-expr))))
                                                                    (interp (bool (or (interp-val (op-bool-larg tcfwael-expr)) (interp-val (op-bool-rarg tcfwael-expr)))) env)
                                                                    (error "error: Hay algun argumento que no es de tipo boolV"))]
                            [(eq? 'and (op-bool-f tcfwael-expr)) (if (eq? #t (argsBoolV (list (op-bool-larg tcfwael-expr) (op-bool-rarg tcfwael-expr))))
                                                                     (interp (bool (and (interp-val (op-bool-larg tcfwael-expr)) (interp-val (op-bool-rarg tcfwael-expr)))) env)
                                                                     (error "error: Hay algun argumento que no es de tipo boolV"))])]
    [(branch? tcfwael-expr) (if (eq? #t (eq? boolV (check-type (branch-test tcfwael-expr))))
                                (if (eq? #t (boolV-b (interp (branch-test tcfwael-expr) env)))
                                (interp (branch-then tcfwael-expr) env)
                                (interp (branch-else tcfwael-expr) env))
                                (error "error: El test no es de tipo boolV"))]
    [(fun? tcfwael-expr) (closureV (fun-params tcfwael-expr) (fun-body tcfwael-expr) env)]
    [(lcons? tcfwael-expr) (let (
                               [l-expr (interp (lcons-l tcfwael-expr) env)]
                               [r-expr (interp (lcons-r tcfwael-expr) env)])
                           (listV (list l-expr r-expr)))]
    [(lcar? tcfwael-expr) (if (eq? #t (eq? listV (check-type (lcar-lst tcfwael-expr)))) (interp (lcons-l (lcar-lst tcfwael-expr)) env)
                              (error "error: La expresion no es de tipo listV"))]
    [(lcdr? tcfwael-expr) (if (eq? #t (eq? listV (check-type (lcdr-lst tcfwael-expr)))) (interp (lcons-r (lcdr-lst tcfwael-expr)) env)
                              (error "error: La expresion no es de tipo listV"))]
    [(app? tcfwael-expr) (let* (
        [apped-fun (app-fun tcfwael-expr)]
        [args (app-args tcfwael-expr)]
        [fun-arg (first (fun-params apped-fun))]
        [env (aSub fun-arg (first args) env)]
        [body (fun-body (app-fun tcfwael-expr))]
        [argumentos (app-args tcfwael-expr)])
                           (interp (substFunc apped-fun env) env)
      )]
    [else ("Error de sintaxis")]
  )
)

;; Funcion auxiliar de interp que convierte a valores de Racket los valores de tipo FWAEL.
;; * Precondiciones: lista de valores de tipo TCFWAEL
;; * Postcondiciones: lista de valores de Racket 
(define (interpOp args env)
  (if (empty? args)
      '()
      (cons (interp-val (car args)) (interpOp (cdr args) env))))

;; Funcion auxiliar de interp que convierte un a valor de Racket el valor de tipo FWAEL.
;; * Precondiciones: valor de tipo TCFWAEL
;; * Postcondciones: valor transformado a Racket
(define (interp-val cfwael-ast)
  (cond
    [(id? cfwael-ast) (error "error: Variable libre")]
    [(num? cfwael-ast) (num-n cfwael-ast)]
    [(bool? cfwael-ast) (bool-b cfwael-ast)]
    [(op? cfwael-ast) (cond
                      [(eq? + (op-f cfwael-ast)) (operSum (map interp-val (op-args cfwael-ast)))]
                      [(eq? - (op-f cfwael-ast)) (operRes (map interp-val (op-args cfwael-ast)))]
                      [(eq? * (op-f cfwael-ast)) (operProd (map interp-val (op-args cfwael-ast)))]
                      [(eq? / (op-f cfwael-ast)) (operDiv (map interp-val (op-args cfwael-ast)))]
                      [(eq? modulo (op-f cfwael-ast)) (operMod (map interp-val (op-args cfwael-ast)))]
                      [(eq? expt (op-f cfwael-ast)) (operExpt (map interp-val (op-args cfwael-ast)))]
                      [(eq? not (op-f cfwael-ast)) (operNot (map interp-val (op-args cfwael-ast)))])]
    [(op-bool? cfwael-ast) (cond
                           [(eq? 'or (op-bool-f cfwael-ast)) (or (interp-val (op-bool-larg cfwael-ast)) (interp-val (op-bool-rarg cfwael-ast)))]
                           [(eq? 'and (op-bool-f cfwael-ast)) (and (interp-val (op-bool-larg cfwael-ast)) (interp-val (op-bool-rarg cfwael-ast)))])]
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


;; Funcion auxiliar de interp que verifica si los argumentos de la expresion son de tipo numV
(define (argsNumV expr)
  (if (eq? '() expr) #t
  (and (eq? numV (check-type (car expr))) (argsNumV (cdr expr)))))

;; Funcion auxiliar de interp que verifica si los argumentos de la expresion son de tipo boolV
(define (argsBoolV expr)
  (if (eq? '() expr) #t
  (and (eq? boolV (check-type (car expr))) (argsBoolV (cdr expr)))))


;; ******************************************************************
