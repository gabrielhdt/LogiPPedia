(define-module (logipp latex-pp)
  #:use-module (ice-9 match)  ; Pattern matching
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-43) ; Vectors
  #:use-module ((logipp extras) #:prefix extras:)
  #:export (pp))

;; Load additionaly bound symbols
; FIXME the alist should be in a separate file, provided by the user
(define sttfa '(("sttfa:sttfa/etap.cst" . "η")))
(define bindings (extras:alist->hash-str-table sttfa))

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

(define (get-symbol symbols uri)
  "Call extras:ref-or-id if symbols is not #f, else return uri."
  (if symbols
      (extras:ref-or-id symbols uri)
      uri))

(define (pp-args ts symbols)
  "Prints ts as a list of arguments."
  (let ((space-pp
         (lambda (_ t)
           (begin
             (display "\\ ")
             (pp/p t symbols)))))
    (vector-for-each space-pp ts)))

(define (pp-annot annot symbols)
  "Prints annot as an annotation, that is ': annot'."
  (match annot
    (#nil #nil)
    (t (begin
         (display ": ")
         (pp/p t symbols)))))

(define (pp-const const symbols)
  "Prints constant ct with symbol c as '(c args)'"
  (match (normalise-object const)
    ((( "c_args" . #() ) ( "c_symb" . csym ))
     (display (sanitise (get-symbol symbols csym))))
    ((( "c_args" . cargs ) ( "c_symb" . csym ))
     (begin
       (format #t "\\left(~a" (sanitise csym))
       (pp-args cargs symbols)
       (display "\\right")))))

(define* (pp-var var #:optional symbols)
  "Prints variable v of symbol v as '(v args)'"
  (match (normalise-object var)
    ((( "v_args" . #() ) ( "v_symb" . vsym ))
     (display (sanitise vsym)))
    ((( "v_args" . vargs ) ( "v_symb" . vsym ))
     (begin
       (format #t "\\left(~a" (sanitise vsym))
       (pp-args vargs symbols)
       (display "\\right)")))))

(define (pp-binder binder symbols)
  "Given a binder with symbol B, bound variable x and body t, prints 'B x.t'"
  (match (normalise-object binder)
    ((( "annotation" . anno )
      ( "b_args" . #() )
      ( "b_symb" . symb )
      ( "body" . t )
      ( "bound" . bound ))
     (begin
       (format #t "\\left(~a ~a" (sanitise symb) (sanitise bound))
       (pp-annot anno symbols)
       (display ", ")
       (pp/p t symbols)
       (display "\\right)")))
    ((( "annotation" . anno )
      ( "b_args" . args )
      ( "b_symb" . symb )
      ( "bound" . bound )
      ( "body" . t ))
     (begin
       (format #t "\\left(\\left(~a ~a" (sanitise symb) (sanitise bound))
       (pp-annot anno symbols)
       (display ", ")
       (pp/p t symbols)
       (display "\\right)")
       (pp-args args symbols)
       (display "\\right)")))))

(define (pp/p ppt symbols)
  "Converts a Scheme representation of a json ppterm to a string with mapping
from uris to symbols as a hashtable."
  (match ppt
    (#("Const" content)
     (pp-const content symbols))
    (#("Binder" content)
     (pp-binder content symbols))
    (#("Var" content)
     (pp-var content symbols))
    (_ (throw 'ill-json))))

;;
;; Public procedure
;;

(define* (pp ppt #:optional symbols)
  "Converts a Scheme representation of a json ppterm to a string."
  (if symbols
      (pp/p ppt (extras:alist->hash-str-table symbols))
      (pp/p ppt #f)))
