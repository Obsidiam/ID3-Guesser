;; This was my own submission to a project in an assignment in CS135

;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname guess) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; A Histogram is a (listof (list Sym Nat))
;; Requires: A symbol can appear in only one pair.

;; An Augmented Histogram (AH) is a (listof (list Sym Nat Nat))
;; Requires: A symbol can appear in only one triple.

;; An Entropy Association List (EAL) is a (listof (list Sym Num))
;; Requires: A symbol can appear in only one pair.

;; A Decision Tree (DT) is one of:
;; * Bool
;; * (list Sym DT DT)

(require "animals.rkt")

;;*********************************************************

;; a

;; (collect-attributes examples) consumes a list of examples and produces
;;      a list of attributes contained in the examples with no duplicates

;; Examples:
(define seen
  (list
   (list 'sparrow 'small 'swims 'flies)
   (list 'duck 'medium 'swims 'flies)
   (list 'sparrow 'small 'swims 'flies 'angry)))

(check-expect (collect-attributes seen) '(medium small swims flies angry))


;; collect-attributes: (listof Example) --> ((listof Sym) -> (listof Sym))
(define (collect-attributes examples)
  (local [(define (remove-duplicate lst)
            (cond
              [(empty? lst) empty]
              [(member? (first lst) (rest lst))
               (remove-duplicate (rest lst))]
              [else (cons (first lst) (remove-duplicate (rest lst)))]))]
    
    (remove-duplicate (cond [(empty? examples) empty]
                            [else 
                             (append (rest (first examples))
                                     (collect-attributes (rest examples)))]))))

;; Tests:
(define sample1
  (list
   (list 'gull 'medium 'swims 'flies)
   (list 'squirrel 'small 'angry)
   (list 'duck 'medium 'flies)
   (list 'goose 'large 'swims 'flies 'angry)
   (list 'squirrel 'small 'angry)
   (list 'sparrow 'small 'swims 'flies)
   (list 'goose 'large 'flies 'angry)
   (list 'crow 'small 'flies 'angry)
   (list 'goose 'medium 'swims 'flies 'angry)
   (list 'crow 'small 'flies 'angry)))
  

(check-expect (collect-attributes '()) '())

(check-expect
 (collect-attributes (list (list 'goose 'small 'swims 'flies 'angry)))
 (list 'small 'swims 'flies 'angry))

(check-expect (collect-attributes sample1)
              (list 'large 'medium 'swims 'small 'flies 'angry))

;;********************************************************************

;; b

;; (split-examples examples symbol) consumes a list of examples and a symbol
;;                       and splits the list of examples on the given symbol

;; Examples:
(define seen1
  (list
   (list 'squirrel 'large 'angry)
   (list 'sparrow 'medium 'flies)
   (list 'duck 'small 'swims 'flies)
   (list 'squirrel 'small)))

(check-expect (split-examples empty 'a) '(() ()))

(check-expect (split-examples seen1 'squirrel)
              (list
               (list (list 'squirrel 'large 'angry)
                     (list 'squirrel 'small))
               (list (list 'sparrow 'medium 'flies)
                     (list 'duck 'small 'swims 'flies))))

(check-expect (split-examples seen1 'small)
              (list
               (list (list 'duck 'small 'swims 'flies)
                     (list 'squirrel 'small))
               (list (list 'squirrel 'large 'angry)
                     (list 'sparrow 'medium 'flies))))


;; split-examples:
;;         (listof Example) Sym --> (list (listof Example) (listof Examples)))
(define (split-examples examples symbol)
  (local [(define (symbol-member? examples symbol)
            (cond [(empty? examples) empty]
                  [(member? symbol (first examples))
                   (cons  (first examples)
                          (symbol-member? (rest examples) symbol))]
                  [else (symbol-member? (rest examples) symbol)]))

          (define (not-symbol-member? examples symbol)
            (cond [(empty? examples) empty]
                  [(not (member? symbol (first examples)))
                   (cons  (first examples)
                          (not-symbol-member? (rest examples) symbol))]
                  [else (not-symbol-member? (rest examples) symbol)]))]

    (list (symbol-member? examples symbol)
          (not-symbol-member? examples symbol))))

;; Tests:
(check-expect (split-examples (list ) 'small) '(() ()))
(check-expect (split-examples (list ) 'squirrel) '(() ()))

(check-expect (split-examples seen1 'swims)
              (list
               (list (list 'duck 'small 'swims 'flies))
               (list
                (list 'squirrel 'large 'angry)
                (list 'sparrow 'medium 'flies)
                (list 'squirrel 'small))))

(check-expect (split-examples seen1 'goose)
              (list
               (list )
               (list
                (list 'squirrel 'large 'angry)
                (list 'sparrow 'medium 'flies)
                (list 'duck 'small 'swims 'flies)
                (list 'squirrel 'small))))

;;*****************************************************************

;; c
    
;; A Histogram is a (listof (list Sym Nat))
;; Requires: A symbol can appear in only one pair.

;; (histogram examples) consumes a list of examples and produces a
;;                      list of attribute/count pairs

;; Examples:
(define seen2
  (list
   (list 'duck 'small 'swims 'flies)
   (list 'gull 'medium 'swims 'flies 'angry)
   (list 'squirrel 'small 'angry)
   (list 'squirrel 'medium 'swims 'angry)
   (list 'goose 'large 'swims 'flies)
   (list 'crow 'small 'angry)))

(check-expect (histogram seen2)
              (list
               (list 'medium 2)(list 'large 1) (list 'swims 4)
               (list 'flies 3) (list 'small 3) (list 'angry 4)))

;; histogram: (listof Example) --> Histogram
(define (histogram examples)
  (local
    [(define (occurence-ttl characteristics)
       (cond [(empty? characteristics) empty]
             [else (cons (list (first characteristics)
                               (length (first (split-examples examples (first characteristics)))))
                         (occurence-ttl (rest characteristics)))]))]
    (occurence-ttl (collect-attributes examples))))

;; Tests:
(define sample2 (list (list 'sparrow 'small 'flies 'angry)))

(define sample3
  (list
   (list 'goose 'large 'swims 'flies 'angry)
   (list 'goose 'large 'swims 'flies 'angry)
   (list 'crow 'medium 'flies 'angry)
   (list 'crow 'small 'flies)
   (list 'sparrow 'medium 'flies)
   (list 'crow 'medium 'flies 'angry)
   (list 'squirrel 'small)
   (list 'goose 'medium 'swims 'flies 'angry)
   (list 'squirrel 'small 'flies 'angry)
   (list 'sparrow 'large 'flies)
   (list 'goose 'large 'swims 'flies 'angry)
   (list 'squirrel 'medium 'angry)
   (list 'gull 'medium 'swims 'flies 'angry)
   (list 'squirrel 'small 'angry)
   (list 'crow 'large 'flies 'angry)
   (list 'goose 'large 'swims 'flies 'angry)
   (list 'duck 'medium 'swims)
   (list 'duck 'small 'swims 'flies 'angry)
   (list 'crow 'small 'flies 'angry)
   (list 'goose 'large 'swims 'flies 'angry)))
  
(check-expect (histogram '()) '())

(check-expect (histogram sample2)
              (list
               (list 'small 1) (list 'flies 1) (list 'angry 1)))

(check-expect (histogram sample3)
              (list
               (list 'medium 7) (list 'small 6) (list 'large 7)
               (list 'swims 9) (list 'flies 16) (list 'angry 15)))

;;********************************************************************

;; An Augmented Histogram (AH) is a (listof (list Sym Nat Nat))
;; Requires: A symbol can appear in only one triple.

;; d

;; (augment-histogram histogram attributes total) consumes a list of all
;;                     attributes and a total for the number of examples
;;                     and produces its augmented histogram

;; Examples:
(check-expect
 (augment-histogram (list (list 'a 100) (list 'c 50)) (list 'a 'b 'c) 200)
 (list (list 'a 100 100) (list 'b 0 200) (list 'c 50 150)))

(check-expect (augment-histogram empty (list 'x 'y) 10)
              (list (list 'x 0 10) (list 'y 0 10)))

;; augment-histogram: Histogram (listof Sym) Nat --> AH
;;      required: total >= any value in the histogram

(define (augment-histogram histogram attributes total)
  (local [(define (not-there? x lst)
            (cond [(empty? lst) true]
                  [(equal? x (first (first lst))) false]
                  [else (not-there? x (rest lst))]))]
    (cond [(empty? attributes) empty]
          [(not-there? (first attributes) histogram)
           (cons (list (first attributes) 0 total)
                 (augment-histogram histogram (rest attributes) total))]
          [else (local [(define (find-attribute x lst)
                          (cond [(equal? x (first (first lst))) (first lst)]
                                [else (find-attribute x (rest lst))]))]
                  (cons
                   (list (first attributes)
                         (second (find-attribute (first attributes) histogram))
                         (- total
                            (second
                             (find-attribute (first attributes) histogram))))
                   (augment-histogram histogram (rest attributes) total)))])))

;; Tests:

(check-expect
 (augment-histogram (histogram sample2)
                    (list 'small 'bites 'medium 'flies 'large 'quack 'angry 'swims)
                    4)
 (list
  (list 'small 1 3)
  (list 'bites 0 4)
  (list 'medium 0 4)
  (list 'flies 1 3)
  (list 'large 0 4)
  (list 'quack 0 4)
  (list 'angry 1 3)
  (list 'swims 0 4)))

(check-expect (augment-histogram (list ) (list ) 0)
              (list ))

(check-expect
 (augment-histogram (histogram sample3)
                    '(chirp large medium small angry smart flies swims) 25)
 (list
  (list 'chirp 0 25)
  (list 'large 7 18)
  (list 'medium 7 18)
  (list 'small 6 19)
  (list 'angry 15 10)
  (list 'smart 0 25)
  (list 'flies 16 9)
  (list 'swims 9 16)))
 
;;**********************************************************************

;; e

;; (entropy positive-counts negative-counts) consumes 2 elements from augmented
;;                                     histograms and produces their entropy

;; Examples:
(check-within (entropy (list 'large 126 59) (list 'large 146 669))
              0.566 0.001)

(check-within (entropy (list 'small 17 168) (list 'small 454 361))
              0.583 0.001)

(check-within (entropy (list 'a 0 100) (list 'b 100 0))
              0.000 0.001)

;; entropy: (list Sym Nat Nat) (list Sym Nat Nat) --> Num
;;   requires: All the Nats the fucntion consumes >= 0
(define (entropy positive-counts negative-counts)
  (local [(define (P n m)
            (cond [(= (+ n m) 0) 0.5]
                  [else (/ n (+ n m))]))
          (define (ep p)
            (cond [(= p 0) 0]
                  [else (* (* -1 p) (log p 2))]))
          (define a (second positive-counts))
          (define b (second negative-counts))
          (define c (third positive-counts))
          (define d (third negative-counts))]
    (+ (* (P (+ a b) (+ c d)) (+ (ep (P a b)) (ep (P b a))))
       (* (P (+ c d) (+ a b)) (+ (ep (P c d)) (ep (P d c)))))))

;;****************************************************************

;; f

;; An Entropy Association List (EAL) is a (listof (list Sym Num))
;; Requires: A symbol can appear in only one pair.

;; (entropy-attributes positive negative) consumes 2 augmneted histograms and
;;                                    computes the entropy of each attribute,
;;                                 producing a list of attribute/entropy pair

;; Examples:
(check-within (entropy-attributes
               (list (list 'large 126 59) (list 'angry 161 24)
                     (list 'small 17 168) (list 'flies 170 15)
                     (list 'swims 162 23) (list 'medium 42 143))
               (list
                (list 'large 146 669) (list 'angry 469 346)
                (list 'small 454 361) (list 'flies 615 200)
                (list 'swims 365 450) (list 'medium 215 600)))

              (list
               (list 'large 0.566) (list 'angry 0.645)
               (list 'small 0.583) (list 'flies 0.670)
               (list 'swims 0.602) (list 'medium 0.690)) 0.001)

;; entropy-attributes: AH AH --> EAL
(define (entropy-attributes positive negative)
  (cond [(empty? positive) empty]
        [else (cons (list (first (first positive))
                          (entropy (first positive) (first negative)))
                    (entropy-attributes (rest positive) (rest negative)))]))

;;***********************************************************************

;; g

;; (best-attribute entropies) consumes a non-empty list of attribute/entropy
;; pairs and produces the attribute with minimum entropy

;; Examples:
(check-expect (best-attribute
               (list
                (list 'large 0.566) (list 'angry 0.645)
                (list 'small 0.583) (list 'flies 0.670)
                (list 'swims 0.602) (list 'medium 0.690)))
              'large)

;; best-attribute: EAL --> Sym
;;  requries: EAL be a non-empty list of attribute/entropy pairs
;;(define (best-attribute entropies)
;;  (local [(define (min-value entropies)
;;            (cond [(empty? (rest entropies)) (second (first entropies))]
;;                  [else (min (second (first entropies))
;;                             (min-value (rest entropies)))]))
;;         
;;          (define (best-attributes-t min-value entropies)
;;            (cond [(equal? min-value (second (first entropies)))
;;                   (first (first entropies))]
;;                  [else (best-attributes-t min-value (rest entropies))]))]
;;  
;;      (best-attributes-t (min-value entropies) entropies)))

(define (best-attribute entropies)
  (local
    [(define (best-attribute-t entropy)
       (cond [(empty? (rest entropy)) (first entropy)]
             [else 
              (cond [(<= (second (first entropy)) (second (best-attribute-t (rest entropy))))
                     (first entropy)]
                    [else (best-attribute-t (rest entropy))])]))]
    (first (best-attribute-t entropies))))


;; Tests:
(check-expect (best-attribute (list (list 'swims 0.678)))
              'swims)

(check-expect (best-attribute (list (list 'large 0.621) (list 'medium 0.532)
                                    (list 'small 0.654) (list 'flies 0.532)))
              'medium)

(check-expect (best-attribute (list (list 'bites 0.345) (list 'flies 0.345)
                                    (list 'barks 0.345) (list 'walks 0.345)
                                    (list 'happy 0.345)))
              'bites)

(check-expect (best-attribute
               (list
                (list 'large 0.666) (list 'happy 0.645)
                (list 'small 0.999) (list 'quack 0.734)
                (list 'angry 0.270) (list 'swims 0.871)
                (list 'flies 0.345) (list 'medium 0.101)))
              'medium)

;;******************************************************************

;; h

;; A Decision Tree (DT) is one of:
;; * Bool
;; * (list Sym DT DT)

;; (build-dt examples label) consumes a list of examples and a label and
;;                           produces the Decision Tree

;; build-dt: (listof Example) Sym --> DT
(define (build-dt examples label)
  (local
    [(define split (split-examples examples label))
     (define att (collect-attributes examples))
     (define pos (first split))
     (define neg (second split))]
    (cond [(empty? pos) false]
          [(empty? neg) true]
          [(and (empty? att)
                (> (length pos) (length neg)))
           true]
          [(empty? att) false]
          [else (local
                  [(define neg-hist (histogram neg))
                   (define neg-aughist (augment-histogram neg-hist
                                                          att
                                                          (length neg)))
                   (define pos-hist (histogram pos))
                   (define pos-aughist (augment-histogram pos-hist
                                                          att
                                                          (length pos)))
                   (define rt (best-attribute (entropy-attributes pos-aughist
                                                                  neg-aughist)))
                   (define rt-split (split-examples examples rt))
                   
                   (define (remove-all attr ex-lst)
                     (cond [(empty? ex-lst) empty]
                           [else (local
                                   [(define (remove-attr lst-attr)
                                      (cond [(symbol=? attr (first lst-attr)) (rest lst-attr)]
                                            [else (cons (first lst-attr)
                                                        (remove-attr (rest lst-attr)))]))]
                                   (cons (cons (first (first ex-lst))
                                               (remove-attr (rest (first ex-lst))))
                                         (remove-all attr (rest ex-lst))))]))

                   (define pos-subtree (build-dt (remove-all rt (first rt-split)) label))
                   (define neg-subtree (build-dt (second rt-split) label))]
                  (cond
                    [(equal? pos-subtree neg-subtree) pos-subtree]
                    [else (list rt pos-subtree neg-subtree)]))])))

;; Tests:
(check-expect (build-dt empty 'one) false)
(check-expect (build-dt '((goose) (goose) (gull)) 'goose) true)
(check-expect (build-dt '((goose) (gull) (gull)) 'goose) false)
(check-expect (build-dt '((emu) (emu) (sparrow) (sparrow)) 'emu) false)
(check-expect (build-dt '((emu fly angry) (emu fly angry) (emu fly angry) (sparrow fly angry) (sparrow fly angry)) 'emu) (list 'fly (list 'angry true false) false))

;;*****************************************************************************

;; i

;; (train-classifier examples label) determines if an example is an instance of label

;; Example:
(check-expect ((train-classifier empty 'goose) '(goose angry swim)) false)

;; train-classifier: (listof Examples) Sym -> ((listof Sym) -> Bool)
(define (train-classifier examples label)
  (local
    [(define decision-tree (build-dt examples label))

     (define (decision-dt dt attr)
       (cond
         [(boolean? dt) dt]
         [(member? (first dt) attr)
          (decision-dt (second dt) attr)]
         [else (decision-dt (third dt) attr)]))

     (define (sort-attr attr)
       (decision-dt decision-tree attr))]
    sort-attr))


;; Tests:
(define sample4 '((gull small swims angry)
                  (gull small swims flies angry)
                  (gull medium swims flies angry)
                  (goose large swims flies angry)
                  (sparrow small flies angry)))
(check-expect ((train-classifier sample4 'emu) '(large feathers emuish)) false)
(check-expect ((train-classifier sample4 'sparrow) '(small swims)) false)
(check-expect ((train-classifier sample4 'sparrow) '(flies angry small)) true)
(check-expect ((train-classifier sample4 'sparrow) '()) true)
(check-expect ((train-classifier sample4 'gull) '(angry)) false)
                   

