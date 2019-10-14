-module(functions).
% -export([dopasowanie/2, help_me/1, beach/1, mniejsza/2,wartosc_bezwzgledna/1, trzeci/1, fac_r/1, fac_i/1, fac_i/2, last_el/1, nalezy_i/2, polacz_r/2, polacz_l/3]).
-compile(export_all).

dopasowanie(X, X) -> true;
dopasowanie(_, _) -> false.

help_me(Animal) ->
    Talk = if Animal == cat  -> "meow";
              Animal == beef -> "mooo";
              Animal == dog  -> "bark";
              Animal == tree -> "bark";
			  true -> "nothing"
           end,
    {Animal, "says " ++ Talk ++ "!"}.
	
beach(Temperature) ->
    case Temperature of
        {celsius, N} when N >= 20, N =< 45 ->
            'favorable';
        {kelvin, N} when N >= 293, N =< 318 ->
            'scientifically favorable';
        {fahrenheit, N} when N >= 68, N =< 113 ->
            'favorable in the US';
        _ ->
            'avoid beach'
    end.
	
mniejsza(X, Y) when X<Y -> true;
mniejsza(_, _) -> false.

wartosc_bezwzgledna(X) when X<0 -> -X;
wartosc_bezwzgledna(X) -> X.

trzeci([_, _, C | _]) -> C;
trzeci(_) -> false.

fac_r(0) -> 1;
fac_r(N) -> N*fac_r(N-1).

fac_i(N) -> fac_i(N, 1).
fac_i(0, A) -> A;
fac_i(N, A) -> fac_i(N-1, A*N).

last_el([]) -> false;
last_el([E]) -> E;
last_el([_|T]) -> last_el(T).

% last_el_r?

nalezy_i(_, []) -> false;
nalezy_i(E, [E|_]) -> true;
nalezy_i(E, [_|T]) -> nalezy_i(E, T).

% nalezy_r?

polacz_r([], E) -> E;
polacz_r([H1|T1], L) -> [H1|polacz_r(T1, L)]. 

polacz_l(L1, L2) -> polacz_l(L1, L2, []).
polacz_l([], L, P) -> P++L;
polacz_l([H|T], L2, P) -> polacz_l(T, L2, P++[H]).

usun_i(L, E) -> usun_i(L, E, []).
usun_i([], _, Akum) -> Akum;
usun_i([E|L], E, Akum) -> usun_i(L, E, Akum);
usun_i([H|L], E, Akum) when H=/=E -> usun_i(L, E, Akum++[H]).

pascal(1) -> [1];
pascal(N) -> 
    Prev =  pascal(N-1),
    L1 = [0|Prev],
    L2 = Prev++[0],
    sum_elements(L1, L2).
    
sum_elements([], _) -> [];
sum_elements(_,  []) -> [];
sum_elements([H1|L1], [H2|L2]) -> [H1+H2|sum_elements(L1,L2)].
