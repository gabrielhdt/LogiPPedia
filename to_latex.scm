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

(define (pp ppt)
  (match ppt
    (#("Const" content)
     (pp_const content))
    (#("Binder" content)
     (pp_binder content))
    (#("Var" content)
     (pp_var content))
    (_ (throw 'ill-json))))

(define (sp_pp t)
  (string-concatenate/shared `("\\ " ,(pp t))))

(define (pp_args ts)
  (for-each sp_pp ts))

(define (pp_annot annot)
  (match annot
    (#nil "")
    (t (string-concatenate/shared `(": " ,(pp t))))))

(define (pp_const ct)
  (match ct
    ((( "c_args" . #() ) ( "c_symb" . csym ))
     csym)
    ((( "c_args" . cargs ) ( "c_symb" . csym ))
     (string-concatenate/shared `("\\left(" ,csym ,(pp_args cargs) "\\right)")))))

(define (pp_var v)
  (match v
    ((( "v_args" . #() ) ( "v_symb" . vsym ))
     vsym)
    ((( "v_args" . vargs ) ( "v_symb" . vsym ))
     (string-concatenate/shared
      `("\\left(" ,vsym ,(pp_args vargs) "\\right)")))))

(define (pp_binder bnd)
  (match bnd
    ((( "b_args" . #() )
      ( "body" . t )
      ( "annotation" . anno )
      ( "bound" . bound )
      ( "b_symb" . symb ))
     (string-concatenate/shared
      `("\\left(" ,symb " " ,bound ,(pp_annot anno) ", "
        ,(pp t) "\\right)")))
    ((( "annotation" . anno )
      ( "b_symb" . symb )
      ( "bound" . bound )
      ( "body" . t )
      ( "b_args" . args ))
     (string-concatenate/shared
      `("\\left(\\left(" ,symb " " ,bound ,(pp_annot anno) ", "
        ,(pp t) "\\right)" ,(pp_args args) "\\right)")))))

(main)
