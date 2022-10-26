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
