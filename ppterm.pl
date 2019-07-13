#!/usr/bin/env swipl

:- initialization(main, main).
:- use_module(library(http/json)).
:- use_module(library(apply)). % For map, foldl &c.

jatom(null).

main([Fname]) :-
    echo(['Loading', Fname]),
    json_of(Fname, Jterm),
    is_json_term(Jterm, [null(jatom(null))]),
    ppterm_to_latex(Jterm),
    nl.

echo([]) :- nl.
echo([Last]) :- !, write(Last), nl.
echo([H|T]) :- write(H), write(' '), echo(T).

%% [json_of(+Fname, -Jterm)] reads file with name [Fname] and creates
%% json term [Jterm]
json_of(Fname, Jterm) :-
    open(Fname, read, Jstream),
    json_read(Jstream, Jterm, [null(jatom(null))]).

%% [latex_of_pptargs(+Ppts, -Ltx_ost)] concatenates list of ppterm
%% arguments [Ppts] to a latex string [Ltx_ost].
latex_of_pptargs(Ppts) :- maplist(sp_pp, Ppts).

%% ppterm_to_latex(+Pp) outputs ppterm [Pp] to stdout
ppterm_to_latex(['Const', Content]) :- const_to_latex(Content).
ppterm_to_latex(['Binder', Content]) :- binder_to_latex(Content).
ppterm_to_latex(['Var', Content]) :- var_to_latex(Content).

%% prints term T with a space before
sp_pp(T) :- format('\\, ~@', [ppterm_to_latex(T)]).

const_to_latex(json([c_symb=Csym, c_args=[]])) :- format('~a', [Csym]).
const_to_latex(json([c_symb=Csym, c_args=Carg])) :-
    format('\\left(~a~@\\right)', [Csym, latex_of_pptargs(Carg)]).
% No arguments version
var_to_latex(json([v_symb=Vsym, v_args=[]])) :-
    format('~a', [Vsym]).
% with arguments
var_to_latex(json([v_symb=Vsym, v_args=Varg])) :-
    format('\\left(~a~@\\right)', [Vsym, latex_of_pptargs(Varg)]).

%% No annotation
binder_to_latex(json([b_symb=Bsym, bound=Boun,
                      annotation=jatom(null), body=Body])) :-
    format('\\left(~a ~a, ~@\\right)', [Bsym, Boun, ppterm_to_latex(Body)]).
binder_to_latex(json([b_symb=Bsym, bound=Boun, annotation=Anno,
                      body=Body])) :-
    Anno = [_, _],
    format('\\left(~a ~a: ~@, ~@\\right)',
           [Bsym, Boun, ppterm_to_latex(Anno), ppterm_to_latex(Body)]).
