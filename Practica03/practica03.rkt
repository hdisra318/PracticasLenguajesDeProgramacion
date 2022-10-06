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
     )]
    )
  )


;; Tipo de dato para representar los AST
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
  



(define (subst fwae-ast sub-id valor)
  (cond
    ; Si la funcion ES un id y puede ser el que esta buscando
    [(id? fwae-ast) (if (= sub-id (id-i fwae-ast))
                        valor
                        fwae-ast
                        )]
    ; La exoresion PUEDE tener ID's y hay que buscar en ellos sub-id
    [(op? fwae-ast) (op (op-f fwae-ast)
        (map (lambda (arg) (subst arg sub-id valor)) (op-args fwae-ast)))]
    ; La expresion NO PUEDE tener ID's
    [else fwae-ast]
    )
)

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







