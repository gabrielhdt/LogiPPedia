#!/usr/bin/env swipl
%% Print a ppterm from stdin to stdout as latex strings. LaTeX strings may
%% contain utf-8

:- initialization(parse, main).
:- use_module(library(http/json)).
:- use_module(library(apply)). % For map, foldl &c.

parse :-
    current_input(Strm),
    json_read(Strm, Jterm),
    is_json_term(Jterm, []),
    pp(Jterm).

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
pp_binder(json([b_symb=Bsym, b_args=Bargs, bound=Boun,
                annotation= @(null), body=Body])) :-
    format('\\left(\\left(~a ~a, ~@\\right)~@\\right)',
           [Bsym, Boun, pp(Body), pp_args(Bargs)]).
% With annotation
pp_binder(json([b_symb=Bsym, b_args=Bargs, bound=Boun, annotation=Anno,
                body=Body])) :-
    Anno = [_, _],
    format('\\left(\\left(~a ~a: ~@, ~@\\right)~@\\right)',
           [Bsym, Boun, pp(Anno), pp(Body), pp_args(Bargs)]).

% FIXME pp_binder contains superfluous parens if b_args is empty
