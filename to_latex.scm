#!/usr/bin/guile -s
!#
(use-modules (json))
(use-modules (ice-9 match))  ; For pattern matching
(use-modules (srfi srfi-43)) ; Vectors library

;; Pretty prints json ppterm to LaTeX strings. All 'pp*' function return
;; strings, the main function prints the string gathered to stdout.

(define (main)
  (let* ((jsppt (json->scm))
         (ppstr (pp jsppt)))
    (begin
      (set-port-encoding! (current-output-port) "UTF-8")
      (set-port-encoding! (current-input-port) "UTF-8")
      (display ppstr))))

(define (normalise-object obj)
  "Put a scheme alist coming from a json object in normal form (that is, sort
first lexicographically)."
  (let ((sp-less ; Comparison on string pairs
         (lambda (p q)
           (string<? (car p) (car q)))))
    (sort obj sp-less)))

(define (pp ppt)
  "Converts a Scheme representation of a json ppterm to a string."
  (match ppt
    (#("Const" content)
     (pp_const content))
    (#("Binder" content)
     (pp_binder content))
    (#("Var" content)
     (pp_var content))
    (_ (throw 'ill-json))))

(define (pp_args ts)
  "Prints ts as a list of arguments."
  (let* ((sp_pp
          (lambda (_ t)
            (string-concatenate/shared `("\\ " ,(pp t)))))
         (spaced (vector-map sp_pp ts)))
    (string-concatenate (vector->list spaced))))

(define (pp_annot annot)
  "Prints annot as an annotation, that is ': annot'."
  (match annot
    (#nil "")
    (t (string-concatenate/shared `(": " ,(pp t))))))

(define (pp_const const)
  "Prints constant ct with symbol c as '(c args)'"
  (match (normalise-object const)
    ((( "c_args" . #() ) ( "c_symb" . csym ))
     csym)
    ((( "c_args" . cargs ) ( "c_symb" . csym ))
     (string-concatenate/shared `("\\left(" ,csym ,(pp_args cargs) "\\right)")))))

(define (pp_var var)
  "Prints variable v of symbol v as '(v args)'"
  (match (normalise-object var)
    ((( "v_args" . #() ) ( "v_symb" . vsym ))
     vsym)
    ((( "v_args" . vargs ) ( "v_symb" . vsym ))
     (string-concatenate/shared
      `("\\left(" ,vsym ,(pp_args vargs) "\\right)")))))

(define (pp_binder binder)
  "Given a binder with symbol B, bound variable x and body t, prints 'B x.t'"
  (match (normalise-object binder)
    ((( "annotation" . anno )
      ( "b_args" . #() )
      ( "b_symb" . symb )
      ( "body" . t )
      ( "bound" . bound ))
     (string-concatenate/shared
      `("\\left(" ,symb " " ,bound ,(pp_annot anno) ", "
        ,(pp t) "\\right)")))
    ((( "annotation" . anno )
      ( "b_args" . args )
      ( "b_symb" . symb )
      ( "bound" . bound )
      ( "body" . t ))
     (string-concatenate/shared
      `("\\left(\\left(" ,symb " " ,bound ,(pp_annot anno) ", "
        ,(pp t) "\\right)" ,(pp_args args) "\\right)")))))

(main)
