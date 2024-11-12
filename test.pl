all_subsets([], []).
all_subsets([Elem|Tail], [Elem|SubTail]) :-
    all_subsets(Tail, SubTail).
all_subsets([_|Tail], SubTail) :-
    all_subsets(Tail, SubTail).