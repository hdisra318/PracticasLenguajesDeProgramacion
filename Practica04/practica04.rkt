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
  [aSub (name symbol?) (value AST?) (bSub Environment ?)])

;; Tipo de dato para los valores FWAEL
(define-type FWAEL-Value
  [numV (n number ?)]
  [boolV (b boolean?)]
  [listV (l (listof FWAEL-Value?))]
  [closureV (param (listof symbol?)) (body AST?) (env Environment?)])

;; ******************************************************************

;; 1. (2 pts). Defina la función (parse sexp), la cual recibe una expresión simbólica (symbolic expression,
;; s-expression); esto es, la expresión puede ser un símbolo o una lista de expresiones simbólicas. (parse sexp)
;; debe construir un Árbol de Sintaxis Abstracta (Abstract Syntax Tree - AST) a partir de la s-expression dada; si
;; es una expresión válida en el lenguaje FWAEL. En otro caso, debe arrojar un error.
;; * Precondiciones: una expresión simbólica (symbolic expression; s-expression).
;; * Postcondiciones: si la s-expression de entrada satisface la gramática del lenguaje FWAEL; devuelve un árbol
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
          [(app)
           (if (equal? 'fun (first (second sexp)))
               (app (parse (second sexp)) (map parse (rest (rest sexp))))
               (error "No hay una funcion")
           )
          ]
     )]
    )
  )




;; 2

;; 3

;; 4

;;   INTERP

(define (interp fwael-expr env)
  (cond
    [(id? fwael-expr) #f]
    ;; TODO LO DEMAS
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
