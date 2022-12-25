/* YOUR CODE HERE (Problem 1, delete the following line) */
reverseL([],[]).

reverseL([H|T],RevL) :- 
    reverseL(T,Temp),
    append(Temp,[H],RevL).

?- reverseL([],X).
?- reverseL([1,2,3],X).
?- reverseL([a,b,c],X).

/* YOUR CODE HERE (Problem 2, delete the following line) */
remove_duplicates(L1, L2) :- 
    remove_duplicates(L1, [], L2).

remove_duplicates([], _, []).

remove_duplicates([H|T], L2, L3) :- 
    member(H, L2), 
    !,
    remove_duplicates(T, L2, L3).
    
remove_duplicates([H|T], L2, [H|L3]) :-
    remove_duplicates(T, [H|L2], L3).

?- remove_duplicates([1,2,3,4,2,3],X).
?- remove_duplicates([1,4,5,4,2,7,5,1,3],X).
?- remove_duplicates([], X).

/* Your CODE HERE (Problem 3, delete the following line) */
assoc_list(L, AL) :- 
    remove_duplicates(L, L1),
    assoc_list(L1, L, [], AL1),
    reverseL(AL1, AL).

assoc_list([], _, AL, AL).

assoc_list([H|T], L, AL1, AL2) :-
    count(H, L, 0, C),
    assoc_list(T, L, [-(H, C)|AL1], AL2).

count(_, [], C, C).

count(X, [X|T], C1, C2) :-
    C3 is C1 + 1,
    count(X, T, C3, C2).

count(X, [Y|T], C1, C2) :-
    X \= Y,
    count(X, T, C1, C2).

?- assoc_list([1], [1-1]).
?- assoc_list([1,1,2,2,2,3,1], [1-3, 2-3, 3-1]).
?- assoc_list([1,1,4,2,2,2,3,1,1,3,1], X).

/* YOUR CODE HERE (Problem 4, delete the following line) */
intersectionL([H|T], L2, [H|T2]) :-
    member(H, L2), 
    !,
    intersectionL(T, L2, T2).

intersectionL([H|T], L2, L3) :-
    \+ member(H,L2), 
    intersectionL(T, L2, L3).

intersectionL([], L2, []).

?- intersectionL([1,2,3,4],[1,3,5,6],[1,3]).
?- intersectionL([1,2,3,4],[1,3,5,6],X).
?- intersectionL([1,2,3],[4,3],[3]).

/* YOUR CODE HERE (Problem 5, delete the following line) */
biggest([H|T], B) :- 
    B is H.

biggestS([H|T], B) :- 
    biggest(T, B).

biggestT([H|T], B) :- 
    biggestS(T, B).

maxL3(L,X) :-
    length(L, N),
    N > 2,
    mergesort(L, L2),
    reverseL(L2, L3),
    biggest(L3, X1),
    biggestS(L3, X2),
    biggestT(L3, X3),
    !,
    X is X1+X2+X3.

?- not(maxL3([1], X)).
?- maxL3([1,2,3,4], 9).
?- maxL3([10,3,2,3,10], X).

/* YOUR CODE HERE (Problem 6, delete the following line) */
append([],Q,Q).
append([H | P], Q, [H | R]) :- append(P, Q, R).
prefix(X,Z) :- append(X,Y,Z).
suffix(Y,Z) :- append(X,Y,Z).

partition([],[],[]).

partition([H], [H], []).

partition(L, P, S) :-
    length(L, N),
    PrefixL is div(N,2),
    SuffixL is N - div(N,2),
    length(P, PrefixL),
    length(S, SuffixL),
    prefix(P, L),
    suffix(S, L).

?- partition([a],[a],[]).
?- partition([1,2,3],[1],[2,3]).
?- partition([a,b,c,d],X,Y).

/* YOUR CODE HERE (Problem 7, delete the following line) */
merge([X|L],[Y|K],[Y|M]) :- 
    merge([X|L],K,M), 
    X>=Y.
    
merge([X|L],[Y|K],[X|M]) :- 
    merge(L,[Y|K],M), 
    X<Y.

merge(L,[],L).

merge([],K,K).

?- merge([],[1],[1]).
?- merge([1],[],[1]).
?- merge([1,3,5],[2,4,6],X).

/* YOUR CODE HERE (Problem 8, delete the following line) */
mergesort([],[]).

mergesort([X],[X]).

mergesort(L,SL) :-
    partition(L, P, S),
    mergesort(P, SP),
    mergesort(S, SS),
    merge(SP, SS, SL).

?- mergesort([3,2,1],X).
?- mergesort([1,2,3],Y).
?- mergesort([],Z).
