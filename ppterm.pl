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

%% [join_with(+Left, +Mid, +Right, -Out)] is true if [Out] is the
%% concatenation of strings [Left] and [Right] with [Mid] between them.
join_with(Left, Mid, Right, Out) :-
    string_concat(Left, Mid, LeMi),
    string_concat(LeMi, Right, Out).

%% [join_with_sp(+Left, +Right, -Out)] true if [Out] is "Left\, Right".
join_with_sp(Left, Right, Out) :- join_with(Left, "\\, ", Right, Out).

%% [latex_of_pptargs(+Ppts, -Ltx_ost)] concatenates list of ppterm
%% arguments [Ppts] to a latex string [Ltx_ost].
latex_of_pptargs(Ppts, Ltx_ost) :-
    maplist(ppterm_to_latex, Ppts, Ltxargs),
    foldl(join_with_sp, Ltxargs, "", Ltx_ost).

%% ppterm_to_latex(+Pp) outputs ppterm [Pp] to stdout
ppterm_to_latex(['Const', Content]) :- const_to_latex(Content).
ppterm_to_latex(['Binder', Content]) :- binder_to_latex(Content).
ppterm_to_latex(['Var', Content]) :- var_to_latex(Content).

const_to_latex(json([c_symb=Csym, c_args=[]])) :- format('~a', [Csym]).
const_to_latex(json([c_symb=Csym, c_args=Carg])) :-
    latex_of_pptargs(Carg, Ltxarg),
    format('\\left(~a\\, ~a\\right)', [Csym, Ltxarg]).
% No arguments version
var_to_latex(json([v_symb=Vsym, v_args=[]])) :- format('~a', [Vsym]).
% with arguments
var_to_latex(json([v_symb=Vsym, v_args=Varg])) :-
    latex_of_pptargs(Varg, Ltxarg),
    format('\\left(~a\\, ~a\\right)', [Vsym, Ltxarg]).

%% No annotation
binder_to_latex(json([b_symb=Bsym, bound=Boun,
                      annotation=jatom(null), body=Body])) :-
    format('\\left(~a ~a, ~@\\right)', [Bsym, Boun, ppterm_to_latex(Body)]).
binder_to_latex(json([b_symb=Bsym, bound=Boun, annotation=Anno,
                      body=Body])) :-
    format('\\left(~a ~a: ~@, ~@\\right)',
           [Bsym, Boun, ppterm_to_latex(Anno), ppterm_to_latex(Body)]).
