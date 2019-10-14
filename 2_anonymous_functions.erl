-module(lab2).
-compile(export_all).

% wynik = 11
foo() -> X=2,
    Bump = fun(X)->X+1 end,
    Bump(10).

% wynik = 13
bar() -> X=3,
    Add = fun(Y) -> X+Y end,
    Add(10).

% ma być error
base1() -> A=1,
    (fun() -> A=2 end)(), A.

% wynik = 1
base2() -> A=1,
    (fun(A) -> A=2 end)(2), A.

a() -> 
    Secret = "pony",
    fun() -> Secret end.

b(F) -> "a/0's password is "++F().

map(_, []) -> [];
map(F, [H|T]) -> [F(H)|map(F,T)].

map_list_comp(F, L) -> [F(E) || E <- L].

myFun(X) when X < 5 -> true;
myFun(X) when X >= 5 -> false.

iterate(S, IsDone, Transform)->
    C = IsDone(S),
    if 
        C -> S;
        true -> S1 = Transform(S), iterate(S1, IsDone, Transform) 
    end.

sqrt(X)->
    iterate(1.0, 
    fun(G)->abs(X-G*G)/X < 0.00001 end,
    fun(G)->(G+X/G)/2.0 end).

% for_range(X)->
    % iterate()

% usun, wstaw, zamien
abs_foldl(List) -> 
    lists:reverse(lists:foldl(fun(A,L) when A<0 -> [-1*A|L]; (A,L) -> [A|L] end, [], List)).

% dokonczyc
abc_rm_el(E, List) ->
    lists:foldl(fun(E, [H|T]) when E=/=H -> T; (E,[H|T]) -> [H|T] end, [], List).