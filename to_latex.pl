#!/usr/bin/env swipl
%% Print a ppterm from stdin to stdout as latex strings. LaTeX strings may
%% contain utf-8
%%
%% Z the order of the json fields matters:
%% [v_symb=_, v_args=_] =\= [v_args=_, v_symb=_]

:- initialization(parse, main).
:- use_module(library(http/json)).
:- use_module(library(apply)). % For map, foldl &c.

parse :-
    current_input(Strm),
    json_read(Strm, Jterm),
    is_json_term(Jterm, []),
    pp(Jterm).

%% pp(+Pp) outputs ppterm [Pp] to stdout
pp(['Const', Content]) :- pp_const(Content).
pp(['Binder', Content]) :- pp_binder(Content).
pp(['Var', Content]) :- pp_var(Content).

%% [pp_args(+Ppts)] prints a list of arguments
pp_args([]).
pp_args(Ts) :- maplist(sp_pp, Ts).

%% sp_pp(+T) prints term T with a latex small space space before
sp_pp(T) :- format('\\, ~@', [pp(T)]).

%% pp_const(+Cst) prints constant Cst
% No arguments version
pp_const(json([c_symb=Csym, c_args=[]])) :- format('~a', [Csym]).
% With arguments
pp_const(json([c_symb=Csym, c_args=Carg])) :-
    format('\\left(~a~@\\right)', [Csym, pp_args(Carg)]).

%% pp_var(+V) prints a bound variable
% No arguments version
pp_var(json([v_symb=Vsym, v_args=[]])) :- format('~a', [Vsym]).
% with arguments
pp_var(json([v_symb=Vsym, v_args=Varg])) :-
    format('\\left(~a~@\\right)', [Vsym, pp_args(Varg)]).

pp_annot(@(null)) :- format('').
pp_annot(Term) :- format(': ~@', [pp(Term)]).

%% pp_binder(+Bd) prints the data Bd that was shipped as ['Binder', Bd].
% Without arguments
pp_binder(json([b_symb=Bsym,
                bound=Boun,
                annotation=Anno,
                body=Body,
                b_args=[]])) :-
    format('\\left(~a ~a~@, ~@\\right)',
           [Bsym, Boun, pp_annot(Anno), pp(Body)]).
% With arguments
pp_binder(json([b_symb=Bsym,
                bound=Boun,
                annotation=Anno,
                body=Body,
                b_args=Bargs])) :-
    format('\\left(\\left(~a ~a~@, ~@\\right)~@\\right)',
           [Bsym, Boun, pp_annot(Anno), pp(Body), pp_args(Bargs)]).
