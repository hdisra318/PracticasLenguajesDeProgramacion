#|

Practica 02

Integrantes:
Hernandez Dorantes Israel - 318206604
Martinez Calzada Diego -  318275457

|#
#lang plai

;; ******************************************************************

;; 1. (1 pto) Tipo de dato abstracto Figura utilizado para trabajar con figuras geométricas con
;; constructores:
;; triangulo - recibe la longitud de los tres lados de un triangulo
;; rectangulo - recibe la altura y base de un rectangulo
;; rombo - recibe el lado, diagonal mayor y diagonal menor de un rombo
;; paralelogramo - recibe los dos lados de un paralelogramo (a es la base) y su altura
;; elipse - recibe el semieje mayor y el semieje menor de una elipse
(define-type Figura
  [triangulo (a number?) (b number?) (c number?)]
  [rectangulo (a number?) (b number?)]
  [rombo (l number?) (D number?) (d number?)]
  [paralelogramo (a number?) (b number?) (h number?)]
  [elipse (a number?) (b number?)]
)


;; ******************************************************************

;; 2. Utilizando el tipo de dato Figura se definen la siguientes funciones:

;; (1.5 pts) Funcion que calcula el perimetro de una Figura dada.
;; * Precondidicones: una instancia de Figura
;; * Postcondiciones: el perimetro de la Figura dada.
;; perimetro: Figura -> number
(define (perimetro f)
        (cond     
          [(triangulo? f) (+ (triangulo-a f) (triangulo-b f) (triangulo-c f))]
          [(rectangulo? f) (+ (* 2 (rectangulo-a f)) (* 2 (rectangulo-b f)))]
          [(rombo? f) (* 4 (rombo-l f))]
          [(paralelogramo? f) (+ (* 2 (paralelogramo-a f)) (* 2 (paralelogramo-b f)))]
          [(elipse? f) (* (* 2 pi) (sqrt (/ (+ (expt (elipse-a f) 2) (expt (elipse-b f) 2)) 2)))])
)



;; (1.5 pts) Funcion que calcula el area de una Figura dada.
;; * Precondidicones: una instancia de Figura
;; * Postcondiciones: el area de la Figura dada.
;; perimetro: Figura -> number
(define (area f)
  (cond
    [(triangulo? f)
     (let ([semiPerim (/ (+ (triangulo-a f) (triangulo-b f) (triangulo-c f)) 2)]) ;; Formula Heron area triangulo 3 lados
       (sqrt (* semiPerim (- semiPerim (triangulo-a f)) (- semiPerim (triangulo-b f))
               (- semiPerim (triangulo-c f)))))]
    [(rectangulo? f) (* (rectangulo-a f) (rectangulo-b f))]
    [(rombo? f) (/ (* (rombo-D f) (rombo-d f)) 2)]
    [(paralelogramo? f) (* (paralelogramo-a f) (paralelogramo-h f))]
    [(elipse? f) (* pi (elipse-a f) (elipse-b f))])
)



;; ******************************************************************

;; 3. Considere una clase de tren de pasajeros conformado por los
;;    siguientes tipos de vagones:

;; (1 pts). El tipo de datos Vagon, que debe incluir 4
;; constructores; uno por cada tipo de vagón descrito
;; anteriormente:
(define-type Vagon
  [dormitorio (camas positive-integer?)]
  [locomotora (p positive-integer?)]
  [pasajeros  (cap positive-integer?)]
  [restaurante (mesas positive-integer?) (personal positive-integer?)])



;; (1 pto). El tipo de datos Tren, que modela trenes conforme a las
;; condiciones descritas anteriormente (vea la descripción).
(define-type Tren
  [tren-loc (vagon locomotora?)]
  [tren (loci locomotora?) (resto Tren?) (locd locomotora?)]
  [tren-t (vagon Vagon?) (resto Tren?)]
  [tren-f (resto Tren?) (vagon Vagon?)])



;; ******************************************************************

;; 4. Utilizando el tipo de datos Tren, defina las siguientes funciones:


;; (1 pto). La función (num-pasajeros tren) que calcula el número de
;; pasajeros máximo que pueden abordar el tren.

;; num-pasajeros: Tren -> positive-integer

;; * Precondiciones: un tren que satisface las condiciones establecidas
;;   en el ejercicio anterior.

;; * Postcondiciones: la suma de las capacidades máximas de sus vagones
;;   de pasajeros.

;; Ejemplos de funcion
;; (num-pasajeros (tren-f (tren-f (tren-f (tren-loc (locomotora 1)) (pasajeros 10)) (restaurante 5 2)) (dormitorio 10)))
;; (num-pasajeros (tren-f (tren-f (tren-loc (locomotora 1)) (pasajeros 10)) (pasajeros 20)))

(define (num-pasajeros tren)
  (cond
    [(tren-loc? tren) 0]
    [(tren? tren) (let [(resto (tren-resto tren))]
                  (num-pasajeros resto))]
    [(tren-t? tren) (let ([vagon (tren-t-vagon tren)]
                          [resto (tren-t-resto tren)])
                          (+ (if (pasajeros? vagon)
                                 (pasajeros-cap vagon)
                                 0)
                             (num-pasajeros resto)))]
    [(tren-f? tren) (let ([vagon (tren-f-vagon tren)]
                          [resto (tren-f-resto tren)])
                          (+ (if (pasajeros? vagon)
                                 (pasajeros-cap vagon)
                                 0)
                             (num-pasajeros resto)))]))



;; (1 pto). La función (arrastre-usado tren) que calcula el porcentaje
;; de la potencia de arrastre utilizada del tren. Debe regresar valores
;; mayores a 100 si la capacidad de arrastre de las locomotoras no es
;; suficiente para mover el resto de los vagones (y este resultado debe
;; ser la proporción de arrastre que excede la capacidad de las
;; locomotoras).

;; arrastre-usado: Tren -> number

;; * Precondiciones: un tren que satisface las condiciones establecidas
;;   en el ejercicio anterior.

;; * Postcondiciones: la proporcion del total de la capacidad de
;;   arrastre de todas las locomotoras respecto al numero de vagones
;;   no-locomotoras como porcentaje numérico.

;; Ejemplos:
;; (arrastre-usado (tren-f (tren-f (tren-f (tren-loc (locomotora 1)) (pasajeros 10)) (restaurante 5 2)) (dormitorio 10)))

(define (arrastre-usado tren)
  (/ (* (cuenta-vagones-noloc tren) 100 ) (ac-potloc tren)))


;; (ac-potloc) Funcion auxiliar de la funcion arrastre-usado que calcula la potencia de todas las
;; locomotoras.
(define (ac-potloc tren)
  (cond
    [(tren-loc? tren) (let ([vagon (tren-loc-vagon tren)])
         (if (locomotora? vagon)
             (locomotora-p vagon)
              0))]
    [(tren? tren) (let (
         [loci (tren-loci tren)]
         [locd (tren-locd tren)]
         [restot (tren-resto tren)])
         (+ (locomotora-p loci)
            (locomotora-p locd)
            (ac-potloc restot)))]
    [(tren-t? tren) (let (
         [vagon (tren-t-vagon tren)]
         [restot (tren-t-resto tren)])
         (+ (ac-potloc restot)
         (if (locomotora? vagon)
             (locomotora-p vagon)
             0)))]
    [(tren-f? tren) (let (
         [restot (tren-f-resto tren)]
         [vagon (tren-f-vagon tren)])
         (+ (ac-potloc restot)
            (if (locomotora? vagon)
                (locomotora-p vagon)
                0)))])
  )

;; (cuenta-vagones-noloc) Funcion auxiliar de la funcion arrastre-usado que regresa el numero de
;; vagones sin contar las locomotoras.
(define (cuenta-vagones-noloc tren)
  (cond
    [(tren-loc? tren)(let (
         [vagon (tren-loc-vagon tren)])
         (if (locomotora? vagon)
             0
             1))]
         [(tren? tren)(let (
              [loci (tren-loci tren)]
              [locd (tren-locd tren)]
              [restot (tren-resto tren)])
              (cuenta-vagones-noloc restot))]
         [(tren-t? tren) (let (
              [vagon (tren-t-vagon tren)]
              [resto (tren-t-resto tren)])
              (+ (cuenta-vagones-noloc resto)
              (if (locomotora? vagon)
                  0
                  1)))]
         [(tren-f? tren)(let (
              [vagon (tren-f-vagon tren)]
              [restot (tren-f-resto tren)])
              (+ (cuenta-vagones-noloc restot)
              (if(locomotora? vagon)
                 0
                 1)))])
  )



;; (1 pto). La funcion (sin-cama tren) que calcula el numero de
;; pasajeros que quedarian sin cama durante; de acuerdo al total de
;; pasajeros y camas en el tren. Considere que las camas son
;; individuales.

;; sin-cama: Tren -> nonnegative-integer

;; * Precondiciones: un tren que satisface las condiciones establecidas
;;   en el ejercicio anterior.

;; * Postcondiciones: el número maximo de pasajeros que excede la
;;   capacidad del total de los vagones dormitorio.

;; Ejemplos:
;; (sin-cama (tren-f (tren-f (tren-f (tren-loc (locomotora 1)) (pasajeros 10)) (restaurante 5 2)) (dormitorio 10)))
;; (sin-cama (tren-f (tren-f (tren-loc (locomotora 1)) (pasajeros 10)) (pasajeros 20)))

(define (sin-cama tren)
  (no-negativo (- (num-pasajeros tren) (num-dormitorios tren))))


;; (num-dormitorios) Funcion auxiliar de la funcion sin-cama que calcula el numero de
;; dormitorios.
(define (num-dormitorios tren)
  (cond
    [(tren-loc? tren) 0]
    [(tren? tren) (let [(resto (tren-resto tren))]
                  (num-dormitorios resto))]
    [(tren-t? tren) (let ([vagon (tren-t-vagon tren)]
                          [resto (tren-t-resto tren)])
                          (+ (if (dormitorio? vagon)
                                 (dormitorio-camas vagon)
                                 0)
                             (num-dormitorios resto)))]
    [(tren-f? tren) (let ([vagon (tren-f-vagon tren)]
                          [resto (tren-f-resto tren)])
                          (+ (if (dormitorio? vagon)
                                 (dormitorio-camas vagon)
                                 0)
                             (num-dormitorios resto)))]))

;; (no-negativo) Funcion auxliar de la funcion sin-cama que dado un numero, si es negativo
;; regresa 0, si no regresa el mismo numero.
(define (no-negativo n)
  (if (negative? n)
      0
      n))



;; (1 pto). La funcion (max-comensales tren) que determina el numero
;; maximo de pasajeros que pueden ser atendidos al mismo tiempo en los
;; vagones restaurante del tren. Considere que una mesa puede albergar
;; 4 pasajeros y un personal de servicio puede atender a 8 pasajeros.
;; Tome en cuenta que la capacidad de las mesas y del personal se
;; limitan entre si: la capacidad del personal puede exceder la
;; capacidad de las mesas, pero no es posible servir a un pasajero sin
;; mesa; como puede haber mesas suficientes para todos los pasajeros
;; pero puede que no se cuente con personal suficiente para atenderlos
;; a todos.

;; max-comensales: Tren -> nonnegative-integer

;; * Precondiciones: un tren que satisface las condiciones establecidas
;;   en el ejercicio anterior.
;; * Postcondiciones: el maximo de pasajeros que pueden ser atendidos
;;   en un vagon restaurante; que esta limitado por su numero de mesas
;;   o de personal de servicio.

;; Ejemplos:
;; (max-comensales (tren-f (tren-f (tren-f (tren-loc (locomotora 1)) (pasajeros 10)) (restaurante 5 2)) (dormitorio 10)))
;; (max-comensales (tren-f (tren-f (tren-loc (locomotora 5)) (pasajeros 20)) (restaurante 10 5)))

(define (max-comensales tren)
  (let* ([max-por-mesas (* 4 (num-mesas tren))]
         [max-por-personal (* 8 (num-personal tren))])
    (if(> max-por-mesas max-por-personal)
       max-por-personal
       max-por-mesas)))


;; (num-mesas) Funcion auxiliar de la funcion max-comensales que calcula el numero de mesas.
(define (num-mesas tren)
  (cond
    [(tren-loc? tren) 0]
    [(tren? tren) (let [(resto (tren-resto tren))]
                  (num-mesas resto))]
    [(tren-t? tren) (let ([vagon (tren-t-vagon tren)]
                          [resto (tren-t-resto tren)])
                          (+ (if (restaurante? vagon)
                                 (restaurante-mesas vagon)
                                 0)
                             (num-mesas resto)))]
    [(tren-f? tren) (let ([vagon (tren-f-vagon tren)]
                          [resto (tren-f-resto tren)])
                          (+ (if (restaurante? vagon)
                                 (restaurante-mesas vagon)
                                 0)
                             (num-mesas resto)))]))

;; (num-personal) Funcion auxiliar de la funcion max-comensales que calcula el numero de personal.
(define (num-personal tren)
  (cond
    [(tren-loc? tren) 0]
    [(tren? tren) (let [(resto (tren-resto tren))]
                  (num-personal resto))]
    [(tren-t? tren) (let ([vagon (tren-t-vagon tren)]
                          [resto (tren-t-resto tren)])
                          (+ (if (restaurante? vagon)
                                 (restaurante-personal vagon)
                                 0)
                             (num-personal resto)))]
    [(tren-f? tren) (let ([vagon (tren-f-vagon tren)]
                          [resto (tren-f-resto tren)])
                          (+ (if (restaurante? vagon)
                                 (restaurante-personal vagon)
                                 0)
                             (num-personal resto)))]))



;; ******************************************************************

;; 5. (1.5 pts) Funciones que realizan una prueba unitaria de cada uno de los ejercicios en la practica

;; Funciones que realizan las pruebas unitarias para el tipo de datos Figura:
;; Funcion que realiza la prueba unitaria de la funcion perimetro para triangulo
(define (prueba-perimetro-triangulo)
  (test (perimetro (triangulo 7 9 11)) 27))

;; Funcion que realiza la prueba unitaria de la funcion perimetro para rectangulo
(define (prueba-perimetro-rectangulo)
  (test (perimetro (rectangulo 12 6)) 36))

;; Funcion que realiza la prueba unitaria de la funcion perimetro para rombo
(define (prueba-perimetro-rombo)
  (test (perimetro (rombo 17 30 16)) 68))

;; Funcion que realiza la prueba unitaria de la funcion perimetro para paralelogramo
(define (prueba-perimetro-paralelogramo)
  (test (perimetro (paralelogramo 7 17 5)) 48))

;; Funcion que realiza la prueba unitaria de la funcion perimetro para elipse
(define (prueba-perimetro-elipse)
  (test (perimetro (elipse 9.45 4.87)) 47.232))

;; Funcion que realiza la prueba unitaria de la funcion area para triangulo
(define (prueba-area-triangulo)
  (test (area (triangulo 7 9 11)) 31.42))

;; Funcion que realiza la prueba unitaria de la funcion area para rectangulo
(define (prueba-area-rectangulo)
  (test (area (rectangulo 12 6)) 72))

;; Funcion que realiza la prueba unitaria de la funcion area para rombo
(define (prueba-area-rombo)
  (test (area (rombo 17 30 16)) 240))

;; Funcion que realiza la prueba unitaria de la funcion area para paralelogramo
(define (prueba-area-paralelogramo)
  (test (area (paralelogramo 7 17 5)) 35))

;; Funcion que realiza la prueba unitaria de la funcion area para elipse
(define (prueba-area-elipse)
  (test (area (elipse 9.45 4.87)) 144.58))

;; Funciones que realizan las pruebas unitarias para los tipos de datos Vagon y Tren:
;; Funcion que realiza la prueba unitaria de la funcion num-pasajeros
(define (prueba-num-pasajeros)
  (test (num-pasajeros (tren-f (tren-f (tren-f (tren-loc (locomotora 1)) (pasajeros 15))
                                               (dormitorio 30)) (pasajeros 15))) 30))

;; Funcion que realiza la prueba unitaria de la funcion arrastre-usado
(define (prueba-arrastre-usado)
  (test (arrastre-usado (tren-f (tren-f (tren-f (tren-f (tren-loc (locomotora 2)) (pasajeros 20))
                                                (dormitorio 15)) (restaurante 10 10)) (dormitorio 5))) 200))

;; Funcion que realiza la prueba unitaria de la funcion sin-cama
(define (prueba-sin-cama)
  (test (sin-cama (tren-f (tren-f (tren-f (tren-loc (locomotora 1)) (pasajeros 10)) (dormitorio 10))
                          (pasajeros 5))) 5))

;; Funcion que realiza la prueba unitaria de la funcion max-comensales
(define (prueba-max-comensales)
  (test (max-comensales (tren-f (tren-f (tren-f (tren-loc (locomotora 2)) (dormitorio 20)) (pasajeros 20))
                                (restaurante 5 5))) 20))