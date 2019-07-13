#!/usr/bin/env swipl
%% Print ppterms to stdout as latex strings.  LaTeX strings may
%% contain utf-8

:- initialization(main, main).
:- use_module(library(http/json)).
:- use_module(library(apply)). % For map, foldl &c.

jatom(null).

%% Entry point, accepts one file name on argv
main([Fname]) :-
    json_of(Fname, Jterm),
    is_json_term(Jterm, [null(jatom(null))]),
    pp(Jterm),
    nl.

%% [json_of(+Fname, -Jterm)] reads file with name [Fname] and creates
%% json term [Jterm]
json_of(Fname, Jterm) :-
    open(Fname, read, Jstream),
    json_read(Jstream, Jterm, [null(jatom(null))]).

%% [pp_args(+Ppts, -Ltx_ost)] concatenates list of ppterm
%% arguments [Ppts] to a latex string [Ltx_ost].
pp_args(Ts) :- maplist(sp_pp, Ts).

%% pp(+Pp) outputs ppterm [Pp] to stdout
pp(['Const', Content]) :- pp_const(Content).
pp(['Binder', Content]) :- pp_binder(Content).
pp(['Var', Content]) :- pp_var(Content).

%% sp_pp(+T) prints term T with a latex small space space before
sp_pp(T) :- format('\\, ~@', [pp(T)]).

%% pp_const prints a constant
% No arguments version
pp_const(json([c_symb=Csym, c_args=[]])) :- format('~a', [Csym]).
% With arguments
pp_const(json([c_symb=Csym, c_args=Carg])) :-
    format('\\left(~a~@\\right)', [Csym, pp_args(Carg)]).

%% pp_var prints a bound variable
% No arguments version
pp_var(json([v_symb=Vsym, v_args=[]])) :-
    format('~a', [Vsym]).
% with arguments
pp_var(json([v_symb=Vsym, v_args=Varg])) :-
    format('\\left(~a~@\\right)', [Vsym, pp_args(Varg)]).

%% pp_binder(+Bd) prints the data Bd that was shipped as ['Binder', Bd].
% No annotation
pp_binder(json([b_symb=Bsym, bound=Boun,
                annotation=jatom(null), body=Body])) :-
    format('\\left(~a ~a, ~@\\right)', [Bsym, Boun, pp(Body)]).
% With annotation
pp_binder(json([b_symb=Bsym, bound=Boun, annotation=Anno,
                body=Body])) :-
    Anno = [_, _],
    format('\\left(~a ~a: ~@, ~@\\right)',
           [Bsym, Boun, pp(Anno), pp(Body)]).
