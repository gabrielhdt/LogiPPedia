#!/usr/bin/env swipl

:- initialization(main, main).
:- use_module(library(http/json)).
:- use_module(library(apply)). % For map, foldl &c.

main([Fname]) :-
    echo(['Loading', Fname]),
    json_of(Fname, Jterm),
    is_json_term(Jterm),
    ppterm_to_latex(Jterm),
    nl.

echo([]) :- nl.
echo([Last]) :- !, write(Last), nl.
echo([H|T]) :- write(H), write(' '), echo(T).

%% [json_of(+Fname, -Jterm)] reads file with name [Fname] and creates
%% json term [Jterm]
json_of(Fname, Jterm) :-
    open(Fname, read, Jstream),
    json_read(Jstream, Jterm).

%% [latex_of_pptargs(+Ppts, -Ltx_ost)] concatenates list of ppterm
%% arguments [Ppts] to a latex string [Ltx_ost].
latex_of_pptargs(Ppts, Ltx_ost) :-
    maplist(ppterm_to_latex, Ppts, Ltxargs),
    foldl(string_concat, Ltxargs, "", Ltx_ost).
%% ppterm_to_latex(+Pp) outputs ppterm [Pp] to stdout
ppterm_to_latex(['Const', Content]) :-
    const_to_latex(Content).
ppterm_to_latex(['Binder', Content]) :-
    binder_to_latex(Content).
ppterm_to_latex(['Var', _]) :- !.
const_to_latex(json([c_symb=Csym, c_args=Carg])) :-
    latex_of_pptargs(Carg, Ltxarg),
    format('\\left(~a\\, ~a\\right)', [Csym, Ltxarg]).
binder_to_latex(json([])) :- !.
