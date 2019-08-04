%read input
read_input(File, K, N, Q, XTickets, YTickets) :-
    open(File, read, Stream),
    read_line(Stream, [K, N, Q]),
    read_tickets(Stream, N, XTickets),
    read_tickets(Stream, Q, YTickets) .

read_line(Stream, L) :-
    read_line_to_codes(Stream, Line),
    atom_codes(Atom, Line),
    atomic_list_concat(Atoms, ' ', Atom),
    maplist(atom_number, Atoms, L).

read_tickets(_, 0, Tickets) :-
  Tickets = [] .
read_tickets(Stream, L, Tickets) :-
  NewL is L-1,
  read_ticket(Stream, T),
  read_tickets(Stream, NewL, RestTickets),
  !,
  Tickets = [T| RestTickets] .

read_ticket(Stream, Ticket) :-
    read_line_to_codes(Stream, Line),
    atom_codes(Atom, Line),
    atom_chars(Atom, Chars), %use char to read tickets starting with 0
    maplist(atom_number, Chars, L),
    reverse(L,Ticket). %store ticket in reverse

%input: [tries], K (Key),
%output: Key, Value, SubTrie of child
%Key = -1, Value = -1, SubTrie = null, if not found
trie_find_child([], _, Key, Value, SubTries) :-
  Key = -1,
  Value = -1,
  SubTries = null .
trie_find_child([trie(K,V,T) | _], K, Key, Value, SubTries) :-
  Key = K,
  Value = V,
  SubTries = T .
trie_find_child([_ | Children], K, Key, Value, SubTries) :-
  trie_find_child(Children, K, Key, Value, SubTries) .

%input: key
%output: trie containing only this key.
trie_build_new([X], Trie) :-
  Trie = trie(X, 1, []) .
trie_build_new([X | L], Trie) :-
  trie_build_new(L, SubTrie),
  !,
  Trie = trie(X, 1, [SubTrie]) .

trie_insert(Trie, [], Trie).
trie_insert(trie(K,V, Children), [H| T], UpdatedTrie) :-
  trie_find_child(Children, H, Key, Value, SubTries),
  ( Key = -1 -> %if child not found
    trie_build_new([H| T], SubTrie),
    UpdatedChildren = [SubTrie| Children],
    UpdatedTrie = trie(K,V,UpdatedChildren)
  ; %if child found
    NewValue is Value + 1,
    select(trie(Key, Value, SubTries), Children, ChildDeleted), %delete child from list
    trie_insert(trie(Key, NewValue, SubTries), T, UpdatedChild), !,%update child
    UpdatedChildren = [UpdatedChild| ChildDeleted], %append updated child
    UpdatedTrie = trie(K, V, UpdatedChildren)
  ) .

%perform a single query.
%input: Y (ticket), Trie
%ouput: Money for ticket Y
query_trie(_, Depth, trie(_, V, []), Money, _) :-
  Money is V * (2 ** Depth - 1) .

query_trie([Y| YS], 0, trie(_,_,SubTries), Money, Num) :-
  trie_find_child(SubTries, Y, Key, Value, ChildSubTries),
  ( Key = -1 ->
    Money is 0,
    Num is 0
    ;
    Num = Value,
    query_trie(YS, 1, trie(Key, Value, ChildSubTries), Money, _)
    ,!
  ) .

query_trie([Y| YS], Depth, trie(_, V, SubTries), Money, _) :-
  trie_find_child(SubTries, Y, ChildKey, ChildValue, ChildSubTries),
  ( ChildKey = -1 -> %child not found
    Power is (powm(2, Depth, 1000000007) - 1) mod 1000000007,
    Money is (V * Power) mod 1000000007
  ; %child found
    NewDepth is Depth + 1,
    query_trie(YS, NewDepth, trie(ChildKey, ChildValue, ChildSubTries), ChildMoney, _),
    Power is (powm(2, Depth, 1000000007) - 1) mod 1000000007,
    Money is (((V - ChildValue) * Power) mod 1000000007 + (ChildMoney mod 1000000007)) mod 1000000007
  ).

%perform queries for all tickets
%input: list of tickets, Trie, HelpList = []
%output: AnswerList
query_tickets([], _, HelpList, AnswerList) :-
  AnswerList = HelpList .
query_tickets([T | Tickets], Trie, HelpList, AnswerList) :-
  query_trie(T, 0, Trie, Money, Num),
  NewList = [[Num, Money]| HelpList],
  query_tickets(Tickets, Trie, NewList, AnswerList).

build_trie([], Trie, Trie) .
build_trie([X| XS], Trie, ResultTrie) :-
  trie_insert(Trie, X, InsertedXTrie),
  build_trie(XS, InsertedXTrie, ResultTrie) .

lottery(File, AnswerList) :-
  read_input(File, _, _, _, XTickets, YTickets),
  build_trie(XTickets, trie(42,42,[]), Trie), %root is a dummy node
  query_tickets(YTickets, Trie, [], L),
  reverse(L, AnswerList) .
