#!/usr/bin/env swipl

:- initialization(main, main).
:- use_module(library(http/json)).
:- use_module(library(apply)).

main([Fname]) :-
    echo(['Loading', Fname]),
    json_of(Fname, Jterm),
    is_json_term(Jterm),
    ppterm_to_latex(Jterm, latex),
    nl.

echo([]) :- nl.
echo([Last]) :- !, write(Last), nl.
echo([H|T]) :- write(H), write(' '), echo(T).

json_of(Fname, Jterm) :-
    open(Fname, read, Jstream),
    json_read(Jstream, Jterm).

ppterm_to_latex(['Const', Content], Ltx) :-
    const_to_latex(Content, Ltx).
ppterm_to_latex(['Binder', Content], Ltx) :-
    binder_to_latex(Content, Ltx).
ppterm_to_latex(['Var', _], _) :- !.
const_to_latex(json([c_symb=Csym, c_args=Carg]), _) :-
    maplist(ppterm_to_latex, Carg, Ltxargs),
    foldl(string_concat, Ltxargs, "", Ltxarg),
    format('\\left(~a\\, ~a)', [Csym, Ltxarg]).
binder_to_latex(json([]), _) :- !.

