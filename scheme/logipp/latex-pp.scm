(define-module (logipp latex-pp)
  #:use-module (ice-9 match)  ; Pattern matching
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-43) ; Vectors
  #:use-module ((logipp extras) #:prefix extras:)
  #:export (pp))

(define (normalise-object obj)
  "Put a scheme alist coming from a json object in normal form (that is, sort
lexicographically on keys)."
  (let ((sp-less ; Comparison on string pairs
         (lambda (p q)
           (string<? (car p) (car q)))))
    (sort obj sp-less)))

(define (sanitise id)
  "Sanitise names for latex, e.g. inserting backslashes."
  (regexp-substitute/global #f "_" id 'pre "\\_" 'post))

(define (get-symbol uriconv uri)
  "Call extras:ref-or-id if uriconv is not #f, else return uri."
  (if uriconv
      (extras:ref-or-id uriconv uri)
      uri))

(define (pp-args ts uriconv)
  "Prints ts as a list of arguments."
  (let ((space-pp
         (lambda (_ t)
           (begin
             (display "\\ ")
             (pp/p t uriconv)))))
    (vector-for-each space-pp ts)))

(define (pp-annot annot uriconv)
  "Prints annot as an annotation, that is ': annot'."
  (match annot
    (#nil #nil)
    (t (begin
         (display ": ")
         (pp/p t uriconv)))))

(define (pp-const const uriconv)
  "Prints constant ct with symbol c as '(c args)'"
  (match (normalise-object const)
    ((( "c_args" . #() ) ( "c_symb" . csym ))
     (display (sanitise (get-symbol uriconv csym))))
    ((( "c_args" . cargs ) ( "c_symb" . csym ))
     (begin
       (format #t "\\left(~a" (sanitise csym))
       (pp-args cargs uriconv)
       (display "\\right")))))

(define* (pp-var var #:optional uriconv)
  "Prints variable v of symbol v as '(v args)'"
  (match (normalise-object var)
    ((( "v_args" . #() ) ( "v_symb" . vsym ))
     (display (sanitise vsym)))
    ((( "v_args" . vargs ) ( "v_symb" . vsym ))
     (begin
       (format #t "\\left(~a" (sanitise vsym))
       (pp-args vargs uriconv)
       (display "\\right)")))))

(define (pp-binder binder uriconv)
  "Given a binder with symbol B, bound variable x and body t, prints 'B x.t'"
  (match (normalise-object binder)
    ((( "annotation" . anno )
      ( "b_args" . #() )
      ( "b_symb" . symb )
      ( "body" . t )
      ( "bound" . bound ))
     (begin
       (format #t "\\left(~a ~a" (sanitise symb) (sanitise bound))
       (pp-annot anno uriconv)
       (display ", ")
       (pp/p t uriconv)
       (display "\\right)")))
    ((( "annotation" . anno )
      ( "b_args" . args )
      ( "b_symb" . symb )
      ( "bound" . bound )
      ( "body" . t ))
     (begin
       (format #t "\\left(\\left(~a ~a" (sanitise symb) (sanitise bound))
       (pp-annot anno uriconv)
       (display ", ")
       (pp/p t uriconv)
       (display "\\right)")
       (pp-args args uriconv)
       (display "\\right)")))))

(define (pp/p ppt uriconv)
  "Converts a Scheme representation of a json ppterm to a string with mapping
from uris to uriconv as a hashtable."
  (match ppt
    (#("Const" content)
     (pp-const content uriconv))
    (#("Binder" content)
     (pp-binder content uriconv))
    (#("Var" content)
     (pp-var content uriconv))
    (_ (throw 'ill-json))))

;;
;; Public procedure
;;

(define* (pp ppt #:optional uriconv)
  "Converts a Scheme representation of a json ppterm to a string."
  (if uriconv
      (pp/p ppt (extras:alist->hash-str-table uriconv))
      (pp/p ppt #f)))
