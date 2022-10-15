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
          [(with) (with (binding (second sexp) (third sexp)) (parse (rest (rest (rest sexp)))))]
     )]
    )
  )


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

(define (esta-en-lista v ls)
  (if (empty? ls)
      #f
      (if (equal? v (first ls))
          #t
          (esta-en-lista v (rest ls))
      )
  )
)



;; 3. (3pts) Funcion que evalua la expresion FWAE dada.
;; * Precondiciones: una expresion FWAE en su representacion como Arbol de Sintaxis Abstracta (AST).
;; * Postcondiciones: un numero, booleano o función en su representacion como Arbol de Sintaxis Abstracta
;;   (AST) a la que se reduce la expresión FWAE dada tras ser evaluada.
;; interp: AST -> number U boolean U AST-fun
#|(define (interp fwae-ast)
  (cond
    [(with? fwae-ast) (let* (
                             [bdgs (with-bindings fwae-ast)]
                             [primeros-bdgs (())]
                             [ultimo-bdg (last bdgs)])
                             (foldl (lambda (bdg)
                               (subst (with-body fwae-ast) (binding-id bdg) (binding-value bdg)))
                             ; ultimo elemento para foldl
                               ultimo-bdg
                               primeros-bdgs)
                             )]
    )
)

|#


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







