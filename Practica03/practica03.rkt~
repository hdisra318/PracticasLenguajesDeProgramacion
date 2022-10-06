#lang plai

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
      (op operador (map parse (resto opsexp)))
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

(define (interp fwae-ast)
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










