#|

Practica 03

Integrantes:
Hernandez Dorantes Israel - 318206604
Martinez Calzada Diego -  318275457

|#
#lang plai


;; ******************************************************************

;; 1. (3pts) Funcion (parse sexp), la cual recibe una expresion simbolica (symbolic expression,
;; s-expression) y construye un Arbol de Sintaxis Abstracta (Abstract Syntax Tree - AST)
;; a partir de la s-expression si es una expresión valida en el lenguaje FWAE.
;; En otro caso, arroja un error.
;; * Precondiciones: una expresion simbolica (symbolic expression; s-expression).
;; * Postcondiciones: si la s-expression de entrada satisface la gramatica del lenguaje FWAE; devuelve un arbol
;;   de sintaxis abstracta AST, de lo contrario, enviar un error.
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
  (cond
    [(symbol? sexp)
        (case sexp
          [(T) (bool #t)]
          [(F) (bool #f)]
          [else (id sexp)]
          )]
    [(number? sexp) (num sexp)]
    [(list? sexp) (case (first sexp)
          [(+ - * / modulo expt not) (parse-op sexp)]
          [(with) (with (map appBinding (second sexp)) (parse (third sexp)))]
          [(with*) (with* (map appBinding (second sexp)) (parse (third sexp)))]
          [(fun) (fun (second sexp) (funBody (third sexp)))]
          [(app) (app (parse (second sexp)) (map parse (rest (rest sexp))))]
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


;; Tipo de dato para representar el Arbol de Sintaxis Abstracta (AST)
(define-type AST
  [id (i symbol?)]
  [num (n number?)]
  [bool (b boolean?)]
  [op (f procedure?) (args (listof AST?))]
  [with (bindings (listof binding?)) (body AST?)]
  [with* (bindings (listof binding?)) (body AST?)]
  [fun (params (listof symbol?)) (body AST?)]
  [app (fun AST?) (args (listof AST?))]
)

;; Tipo de dato para las ataduras (Bindings)
(define-type Binding
  [binding (id symbol?) (value AST?)]
)
  

;; ******************************************************************

;; 2. (3pts) Funcion que realiza la sustitucion fwae-expr[sub-id:=value];
;; es decir, reemplaza cada ocurrencia de la variable sub-id en la expresión fwae-expr por otra expresión value.
;; * Precondiciones: una expresión FWAE representada en un AST, un símbolo a sustituir y otra expresión FWAE
;;   (AST) para sustituir el símbolo.
;; * Postcondiciones: una expresión FWAE representada como AST con el símbolo sustituido por la segunda expresión
;;   FWAE.
;; subst: AST, symbol, AST -> AST

(define (subst fwae-ast sub-id valor)
  (define (subst-aux op-aux)
    (subst op-aux sub-id valor)
  )
  (define (revisa-named-expr ls un-id)
    (if (empty? ls)
        #f
        (let ([kslajsoa (first ls)])
          (if (equal? (binding-id kslajsoa) un-id)
              #t
              (revisa-named-expr (rest ls) un-id)
          )
      )
    )
  )
  (define (cambia-named-expr ls)
    (binding (binding-id ls) (subst (binding-value ls) sub-id valor))
    ;;((binding-id ls) (subst (binding-value ls) sub-id valor))
  )
  (cond
    ; Si la funcion ES un id y puede ser el que esta buscando
    [(id? fwae-ast) (if (equal? sub-id (id-i fwae-ast))
                        valor
                        fwae-ast
                        )]
    ; La exoresion PUEDE tener ID's y hay que buscar en ellos sub-id
    ;; (map (lambda arg (subst arg sub-id valor)) (op-args fwae-ast)))
    [(op? fwae-ast) (op (op-f fwae-ast)
        (map subst-aux (op-args fwae-ast)))]
    [(with? fwae-ast)
     (if (revisa-named-expr (with-bindings fwae-ast) sub-id)
         (with (map cambia-named-expr (with-bindings fwae-ast)) (with-body fwae-ast))
         (with (map cambia-named-expr (with-bindings fwae-ast)) (subst (with-body fwae-ast) sub-id valor))
     )
    ]
    [(with*? fwae-ast)
     (if (revisa-named-expr (with*-bindings fwae-ast) sub-id)
         (with* (map cambia-named-expr (with*-bindings fwae-ast)) (with*-body fwae-ast))
         (with* (map cambia-named-expr (with*-bindings fwae-ast)) (subst (with*-body fwae-ast) sub-id valor))
     )
    ]
    [(fun? fwae-ast)
     (if (esta-en-lista sub-id (fun-params fwae-ast))
         fwae-ast
         (fun (fun-params fwae-ast) (subst (fun-body fwae-ast) sub-id valor))
         )
    ]
    [(app? fwae-ast)
     (app (subst (app-fun fwae-ast) sub-id valor) (map subst-aux (app-args fwae-ast)))
    ]
    ; La expresion NO puede tener ID's
    [else fwae-ast]
    )
)

;; Funcion auxuliar que dice si un elemento esta en una lista
;; * Precondiciones: una lista ls con el mismo tipo que v.
;; * Postcondiciones: #t si esta, #f en otro caso.
(define (esta-en-lista v ls)
  (if (empty? ls)
      #f
      (if (equal? v (first ls))
          #t
          (esta-en-lista v (rest ls))
      )
  )
)


;; ******************************************************************

;; 3. (3pts) Funcion que evalua la expresion FWAE dada.
;; * Precondiciones: una expresion FWAE en su representacion como Arbol de Sintaxis Abstracta (AST).
;; * Postcondiciones: un numero, booleano o función en su representacion como Arbol de Sintaxis Abstracta
;;   (AST) a la que se reduce la expresión FWAE dada tras ser evaluada.
;; interp: AST -> number U boolean U AST-fun
(define (interp fwae-ast)
  (cond
    [(id? fwae-ast) (error "error: Variable libre")]
    [(num? fwae-ast) fwae-ast]
    [(bool? fwae-ast) fwae-ast]
    [(op? fwae-ast) (cond
                      [(eq? + (op-f fwae-ast)) (operSum (map num-n (map interp (op-args fwae-ast))))]
                      [(eq? - (op-f fwae-ast)) (operRes (map num-n (map interp (op-args fwae-ast))))]
                      [(eq? * (op-f fwae-ast)) (operProd (map num-n (map interp (op-args fwae-ast))))]
                      [(eq? / (op-f fwae-ast)) (operDiv (map num-n (map interp (op-args fwae-ast))))]
                      [(eq? modulo (op-f fwae-ast)) (operMod (map num-n (map interp (op-args fwae-ast))))]
                      [(eq? expt (op-f fwae-ast)) (operExpt (map num-n (map interp (op-args fwae-ast))))]
                      [(eq? not (op-f fwae-ast)) (operNot (map bool-b (map interp (op-args fwae-ast))))])]
    [(with? fwae-ast) (let* (
                             [bdgs (with-bindings fwae-ast)]
                             [cuerpo (with-body fwae-ast)])
                             (interp (evalWith cuerpo bdgs)))]
    [(fun? fwae-ast) fwae-ast]
    [(app? fwae-ast) (let* (
                            [func (app-fun fwae-ast)]
                            [oper (fun-body func)]
                            [params (fun-params func)]
                            [argus (app-args fwae-ast)])
                            (evaluaFunc params oper argus))]
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
      (foldl - 0 l)
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
      (foldl / 1 l)
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

#|
(let* (
                             [bdgs (with-bindings fwae-ast)]
                             [primeros-bdgs (cdr bdgs)]
                             [ultimo-bdg (car bdgs)])
                             (interp (foldl (lambda (bdg)
                               (subst (with-body fwae-ast) (binding-id bdg) (binding-value bdg)));; El lambda solo toma un argumento
                             ; ultimo elemento para foldl
                               ultimo-bdg
                               primeros-bdgs)
                             ))|#

  
;; Funcion auxiliar de interp que evalua la funcion dados los parametros y los argumentos
(define (evaluaFunc params oper argus)
  (cond
    [(not (eq? (length params) (length argus))) (error "error: El numero de parametros es inadecuado")]
    [else (interp (subst-varios oper params argus))]
   )
)


;; Funcion auxiliar de subst-varios para sustituir los valores de los parametros y evaluar la funcion
(define (subst-varios oper ids valores)
  (if (eq? 0 (length ids))
      oper
      (subst-varios (subst oper (first ids) (first valores)) (rest ids) (rest valores))))


;; ******************************************************************

;; 4. (1 pto). Indique con comentarios en todas las invocaciones a las funciones subst (Ejercicio 2) e interp (Ejercicio
;; 3) si su interprete tiene implementada evaluacion glotona o evaluacion perezosa y porque. Un interprete gloton
;; primero evalua los valores asociados a un identificador y despues sustituye estos valores. Un interprete perezoso,
;; primero sustituye el cuerpo de la expresion asociada a un identificador y despues evalua la expresion. Debe evitar
;; mezclar ambas estrategias de evaluacion en su interprete.

#| La composicion de funciones en las que se invoca primero a <subst>,
   hace al interprete <perezoso>.
   Esto se debe a que <en ningun momento se llama a una funcion que
   haga alguna evaluacion, es decir, si como parametro se da una
   operacion que se pueda evaluar no lo hace. Ademas se respeta el
   hecho que la funcion subst solamanete sustituye, y esto tambien
   hace que sea perezoso>
|#

#| La composicion de funciones en las que se invoca primero a <interp>,
   hace al interprete <perezoso>.
   Esto se debe a que <primero hace la sustitucion, despues, si se
   necesita, evalua hasta que sea necesario, es decir, en el momento
   en el que se necesite el valor de un id.>
|#


;; ******************************************************************

;; 5. (1pto Extra). Funcion que calcula el area de una elipse en el lenguaje FWAE.
;; * Precondiciones: dos numeros que representan respectivamente el eje semi-mayor y el eje semi-menor de la
;;   elipse.
;; * Postcondiciones: el area encerrada por la elpise con ejes semi-mayor y semi-menor dados.
;; areaelipse: AST-num, AST-num -> number
(define (areaelipse semi-mayor semi-menor)
  (interp (parse '(app (fun (smay smen) (* 3.1415 smay smen)) semi-mayor semi-menor))))



