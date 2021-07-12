2390
((3) 0 () 3 ((q lib "phc-adt/tagged-structure-low-level.hl.rkt") (q 1466 . 3) (q lib "phc-graph/flexible-with-utils.hl.rkt")) () (h ! (equal) ((c form c (c (? . 0) q tagged-type!)) q (3359 . 7)) ((q form ((lib "phc-graph/traversal.hl.rkt") define-fold)) q (0 . 3)) ((c form c (c (? . 0) q tagged-builder!)) q (1845 . 11)) ((c def c (c (? . 0) q tagged-struct-id?)) q (5195 . 7)) ((c def c (c (? . 2) q floor-log2)) q (1280 . 3)) ((c form c (c (? . 0) q tagged-any-fields-predicate)) q (2561 . 5)) ((c form c (c (? . 0) q has-fields/type)) q (4252 . 3)) ((c def c (c (? . 0) q make-TaggedTop-struct)) c (? . 1)) ((c form c (c (? . 0) q check-remembered-tagged!)) q (1633 . 3)) ((c form c (c (? . 0) q check-remembered-node!)) q (1706 . 3)) ((c def c (c (? . 0) q TaggedTop-struct?)) c (? . 1)) ((c form c (c (? . 0) q split)) q (4397 . 3)) ((c form c (c (? . 0) q tagged-infer-type!)) q (3649 . 7)) ((c form c (c (? . 0) q merge/type)) q (4709 . 4)) ((c def c (c (? . 2) q ceiling-log2)) q (1372 . 3)) ((c form c (c (? . 0) q tagged-predicate!)) q (2643 . 9)) ((c form c (c (? . 0) q tagged-any-predicate!)) q (2426 . 7)) ((c def c (c (? . 0) q has-fields)) q (3924 . 6)) ((c form c (c (? . 0) q λ-tagged-get-field)) q (3891 . 3)) ((c form c (c (? . 0) q split/type)) q (4483 . 3)) ((c form c (c (? . 0) q with+)) q (4871 . 4)) ((c form c (c (? . 0) q with!!)) q (5083 . 4)) ((c form c (c (? . 0) q tagged-get-field)) q (3858 . 3)) ((c form c (c (? . 0) q merge)) q (4577 . 4)) ((c form c (c (? . 0) q tagged-∀-type!)) q (3494 . 7)) ((c form c (c (? . 0) q check-remembered-?!)) q (1777 . 3)) ((c form c (c (? . 0) q tagged-anytag-match!)) q (3216 . 7)) ((c form c (c (? . 0) q tagged-any-fields-type)) q (3781 . 5)) ((c form c (c (? . 0) q tagged-∀-builder!)) q (2058 . 11)) ((c form c (c (? . 0) q with!)) q (4974 . 4)) ((c def c (c (? . 0) q struct:TaggedTop-struct)) c (? . 1)) ((c def c (c (? . 2) q from-bits)) q (1194 . 3)) ((c form c (c (? . 0) q has-fields/common)) q (4192 . 3)) ((c def c (c (? . 0) q TaggedTop-struct)) c (? . 1)) ((c def c (c (? . 2) q to-bits)) q (1110 . 3)) ((c form c (c (? . 0) q change-tag)) q (4319 . 3)) ((c form c (c (? . 0) q tagged-pred-predicate!)) q (2818 . 9)) ((c form c (c (? . 0) q check-remembered-common!)) q (1560 . 3)) ((c form c (c (? . 0) q tagged-infer-builder!)) q (2291 . 7)) ((c form c (c (? . 0) q tagged-match!)) q (3036 . 9))))
syntax
(define-fold function-name type-name whole-type type-to-replaceᵢ ...)

phase 1 procedure
(idx→tree none-wrapper     
          leaf-wrapper     
          node-wrapper     
          vec)         -> r
  none-wrapper : (->d [depth exact-nonnegative-integer?]
                      any/c)
  leaf-wrapper : (->d [i-in-vec exact-nonnegative-integer?]
                      [leaf-idx exact-nonnegative-integer?]
                      any/c)
  node-wrapper : (->d [left any/c]
                      [right any/c]
                      any/c)
  vec : (vectorof exact-nonnegative-integer?)
procedure
(r #:depth depth                
   #:vec-bounds vec-start       
   vec-after-end                
   #:leaf-bounds first-leaf     
   after-last-leaf)         -> any/c
  depth : exact-nonnegative-integer?
  vec-start : exact-nonnegative-integer?
  vec-after-end : exact-nonnegative-integer?
  first-leaf : exact-nonnegative-integer?
  after-last-leaf : exact-nonnegative-integer?
procedure
(to-bits n) -> (listof boolean?)
  n : exact-nonnegative-integer?
procedure
(from-bits n) -> exact-nonnegative-integer?
  n : (listof boolean?)
procedure
(floor-log2 n) -> exact-nonnegative-integer?
  n : exact-positive-integer?
procedure
(ceiling-log2 n) -> exact-nonnegative-integer?
  n : exact-positive-integer?
struct
(struct TaggedTop-struct ()
    #:extra-constructor-name make-TaggedTop-struct)
for-syntax function
(check-remembered-common! #'(name fieldᵢ ...))

for-syntax function
(check-remembered-tagged! #'(name fieldᵢ ...))

for-syntax function
(check-remembered-node! #'(name fieldᵢ ...))

for-syntax function
(check-remembered-?! #'(name fieldᵢ ...))

for-syntax function
(tagged-builder! #'(name [fieldᵢ τᵢ] ...))

 
  name = Identifier
          
 tvarᵢ = Identifier
          
fieldᵢ = Identifier
          
    τᵢ = Type
for-syntax function
(tagged-∀-builder! #'((tvarᵢ ...) name [fieldᵢ τᵢ] ...))

 
  name = Identifier
          
fieldᵢ = Identifier
          
 tvarᵢ = Identifier
          
    τᵢ = Type
for-syntax function
(tagged-infer-builder! #'(name fieldᵢ ...))

 
  name = Identifier
          
fieldᵢ = Identifier
for-syntax function
(tagged-any-predicate! #'(name fieldᵢ ...))

 
  name = Identifier
          
fieldᵢ = Identifier
for-syntax function
(tagged-any-fields-predicate #'name)

 
name = Identifier
for-syntax function
(tagged-predicate! #'(name [fieldᵢ τᵢ] ...))

 
  name = Identifier
          
fieldᵢ = Identifier
          
    τᵢ = Type
for-syntax function
(tagged-pred-predicate! #'(name [fieldᵢ predᵢ] ...))

 
  name = Identifier
          
fieldᵢ = Identifier
          
 predᵢ = (ExpressionOf (→ Any Any : τᵢ))
for-syntax function
(tagged-match! #'(name [fieldᵢ patᵢ] ...))

 
  name = Identifier
          
fieldᵢ = Identifier
          
  patᵢ = Match-Pattern
for-syntax function
(tagged-anytag-match! #'([fieldᵢ patᵢ] ...))

 
fieldᵢ = Identifier
          
  patᵢ = Match-Pattern
for-syntax function
(tagged-type! #'(name [fieldᵢ τᵢ] ...))

 
  name = Identifier
          
fieldᵢ = Identifier
for-syntax function
(tagged-∀-type! #'((tvarᵢ ...) name [fieldᵢ τᵢ] ...))

 
  name = Identifier
          
fieldᵢ = Identifier
for-syntax function
(tagged-infer-type! #'(name fieldᵢ ...))

 
  name = Identifier
          
fieldᵢ = Identifier
for-syntax function
(tagged-any-fields-type #'name)

 
name = Identifier
syntax
(tagged-get-field v f)

syntax
(λ-tagged-get-field f)

for-syntax function
(has-fields stx-fields)
 -> (listof (cons/c identifier?
                    (cons/c identifier?
                            (listof identifier?))))
  stx-fields : (syntax/c (listof identifier?))
for-syntax function
(has-fields/common #'(fieldᵢ ...))

for-syntax function
(has-fields/type #'([fieldᵢ τᵢ] ...))

syntax
(change-tag instance [(tagᵢ fieldᵢⱼ ...) new-tagᵢ] ...)

syntax
(split instance : (U (tagᵢ fieldᵢⱼ ...) ...) requestedₖ ...)

syntax
(split/type #'((U (tagᵢ [fieldᵢⱼ τᵢⱼ] ...) ...) requestedₖ ...))

syntax
(merge instance-a instance-b
       : (U [(tag-aᵢ field-aᵢⱼ ...) (tag-bₖ field-bₖₗ ...)] ...))

syntax
(merge/type #'(U [(tag-aᵢ [field-aᵢⱼ τ-aᵢⱼ] ...)
                  (tag-bᵢ [field-bᵢⱼ τ-bᵢⱼ] ...)] ...))

syntax
(with+ instance : (U (tagᵢ fieldᵢⱼ ...) ...)
      [new-field value] ...)

syntax
(with! instance : (U (tagᵢ fieldᵢⱼ ...) ...)
       [updated-field value] ...)

syntax
(with!! instance : (U (tagᵢ fieldᵢⱼ ...) ...)
        [updated-field value] ...)

for-syntax function
(tagged-struct-id? id)
 -> (or/c #f
          (cons/c (or/c 'tagged 'node)
                  (cons/c identifier
                          (listof identifier))))
  id : any/c

                     | |Definitions          Local binding Anonymous functions
------------------------------------------------------------------------------
            Functions| |define               let           λ
               Macros| |define-syntax        let-syntax    N/A
Type‑level functionsa| |define-type          Let           ∀
    Type‑level macros| |define-type-expander Let           Λ
