(use-modules (json))
(use-modules (ice-9 match))
(use-modules (ice-9 pretty-print))

(define (main)
  (let* ((jsppt (json->scm))
         (ppstr (pp jsppt)))
    (begin
      (set-port-encoding! (current-output-port) "UTF-8")
      (set-port-encoding! (current-input-port) "UTF-8")
      (display ppstr))))

(define (sp-less p q)
  "Lexicographic ordering on first element of pairs p and q."
  (string<? (car p) (car q)))

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
  (let ((sp_pp
         (lambda (t)
           (string-concatenate/shared `("\\ " ,(pp t))))))
    (for-each sp_pp ts)))

(define (pp_annot annot)
  "Prints annot as an annotation, that is ': annot'."
  (match annot
    (#nil "")
    (t (string-concatenate/shared `(": " ,(pp t))))))

(define (pp_const ct)
  "Prints constant ct with symbol c as '(c args)'"
  (match (sort ct sp-less)
    ((( "c_args" . #() ) ( "c_symb" . csym ))
     csym)
    ((( "c_args" . cargs ) ( "c_symb" . csym ))
     (string-concatenate/shared `("\\left(" ,csym ,(pp_args cargs) "\\right)")))))

(define (pp_var v)
  "Prints variable v of symbol v as '(v args)'"
  (match (sort v sp-less)
    ((( "v_args" . #() ) ( "v_symb" . vsym ))
     vsym)
    ((( "v_args" . vargs ) ( "v_symb" . vsym ))
     (string-concatenate/shared
      `("\\left(" ,vsym ,(pp_args vargs) "\\right)")))))

(define (pp_binder bnd)
  "Given a binder with symbol B, bound variable x and body t, prints 'B x.t'"
  (match (sort bnd sp-less)
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
