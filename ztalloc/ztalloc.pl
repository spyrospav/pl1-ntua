read_input(File, N, L) :-
  open(File, read, Stream),
  read_line(Stream, [N]),
  read_lines(Stream, [], N, L) .

read_lines(_, Inputs, 0, Inputs) .
read_lines(Stream, CurInputs, N, Inputs) :-
  read_line(Stream, Line),
  NewInputs = [Line| CurInputs],
  NewN is N-1,
  read_lines(Stream, NewInputs, NewN, Inputs) .

read_line(Stream, L) :-
  read_line_to_codes(Stream, Line),
  atom_codes(Atom, Line),
  atomic_list_concat(Atoms, ' ', Atom),
  maplist(atom_number, Atoms, L).
%implement predicates for a queue:
%input: -
%output: an empty Q
queue_create(Q) :-
  empty_assoc(L),
  Q = (0, 0, L) .

%input: Q
%output: Bool
queue_IsEmpty((Head, Tail,_), Bool) :-
  ( Head = Tail ->
    Bool = true
  ;
    Bool = false
  ) .
%input: Q, Elem
%output: UpdatedQ
queue_push((Head, Tail, Q), Elem, UpdatedQ) :-
  NewTail is Tail + 1,
  put_assoc(NewTail, Q, Elem, NewQ),
  UpdatedQ = (Head, NewTail, NewQ) .

%input: Q
%output: Elem (popped element), UpdatedQ
queue_pop((Head, Tail, Q), Elem, UpdatedQ) :-
  NewHead is Head + 1,
  get_assoc(NewHead, Q, Elem),
  del_assoc(NewHead, Q, Elem, NewQ),
  UpdatedQ = (NewHead, Tail, NewQ) .

node_IsVisited(AssocL, Key, IsVisited) :-
  ( get_assoc(Key, AssocL, _) ->
      IsVisited = true
  ;
      IsVisited = false
  ) .
% update_visited(AssocList, Elem, Move, UpdatedAssocList) :-
%   put_assoc(Elem, AssocList, Move, UpdatedAssocList) .

node_move_h((L, R), Q, AssocList, UpdatedQ, UpdatedAssocList) :-
  NewL is L div 2,
  NewR is R div 2,
  Node = (NewL, NewR),
  node_IsVisited(AssocList, Node, IsVisited),!,
  ( NewL < 1000000, NewR < 1000000, IsVisited = false ->
      put_assoc(Node, AssocList, (L, R), UpdatedAssocList),
      queue_push(Q, Node, UpdatedQ)
    ;
    UpdatedQ = Q,
    UpdatedAssocList = AssocList
  ) .

node_move_t((L, R), Q, AssocList, UpdatedQ, UpdatedAssocList) :-
  NewL is 3*L + 1,
  NewR is 3*R + 1,
  Node = (NewL, NewR),
  node_IsVisited(AssocList, Node, IsVisited),
  ( NewL < 1000000, NewR < 1000000, IsVisited = false ->
      put_assoc(Node, AssocList, (L, R), UpdatedAssocList),
      queue_push(Q, Node, UpdatedQ)
    ;
    UpdatedQ = Q,
    UpdatedAssocList = AssocList
  ) .

% explore_node(Node, Q, AssocList, UpdatedQ, UpdatedAssocList) :-
%   node_move_h(Node, Q, AssocList, NewQ, NewList),!,
%   node_move_t(Node, NewQ, NewList, UpdatedQ, UpdatedAssocList) .

check_node((L, R), (L_out, R_out), IsSolution) :-
  ( L >= L_out, R =< R_out ->
    IsSolution = true
  ;
    IsSolution = false
  ) .
%check if performing a t-move yields a solution.
check_t((L, R), TargetNode, IsSolution) :-
  NewL is 3*L + 1,
  NewR is 3*R + 1,
  check_node((NewL, NewR), TargetNode, IsSolution) .

%check if performing a t-move yields a solution.
check_h((L, R), TargetNode, IsSolution) :-
  NewL is L div 2,
  NewR is R div 2,
  check_node((NewL, NewR), TargetNode, IsSolution) .

build_path((L, _), (PrevL, _), CurAnswer, NewAnswer) :-
  ( PrevL >= L ->
    Move = h
  ;
    Move = t
  ),
  NewAnswer = [Move| CurAnswer] .

find_path(AssocList, CurNode, CurAnswer, Answer) :-
  get_assoc(CurNode, AssocList, Value),!,
  ( Value = null ->
      Answer = CurAnswer,!
  ;
    build_path(CurNode, Value, CurAnswer, NewAnswer),!,
    find_path(AssocList, Value, NewAnswer, Answer),!
  ) .


bfs(TargetNode, Q, AssocList, Answer) :-
  queue_IsEmpty(Q, IsEmpty),
  ( IsEmpty = true ->
      Answer = ['I','M','P','O','S','S','I','B','L','E']
  ;
    queue_pop(Q, (L, R), NewQ),
    check_h((L, R), TargetNode, IsSol1),
    ( IsSol1 = true ->
        Answer = 42
        % find_path(AssocList, (L, R), [h], Answer)
    ;
        check_t((L, R), TargetNode, IsSol2),
      ( IsSol2 = true ->
        Answer = 23
        % find_path(AssocList, (L, R), [t], Answer)
      ;
        node_move_h((L, R), NewQ, AssocList, Q2, List2),!,
        node_move_t((L, R), Q2, List2, Q3, List3),!,
        bfs(TargetNode, Q3, List3, Answer),!
      )
    )
  ) .

solve_query(InitNode, TargetNode, Answer) :-
  check_node(InitNode, TargetNode, IsSol),!,
  (IsSol = true ->
    Answer = 'EMPTY'
  ;
    empty_assoc(L),
    put_assoc(InitNode, L, null, AssocList),
    queue_create(Q),
    queue_push(Q, InitNode, NewQ),
    bfs(TargetNode, NewQ, AssocList, Ans),!,
    atom_codes(Answer, Ans)
  ).

solve(_, 0, Answers, Answers) .
solve([X| XS], N, CurAnswers, Answers) :-
  [Lin, Rin, Lout, Rout] = X,
  solve_query((Lin, Rin), (Lout, Rout), Ans),!,
  NewAnswers = [Ans| CurAnswers],
  NewN is N - 1,!,
  solve(XS, NewN, NewAnswers, Answers),
  ! .

ztalloc(File, Answer) :-
  read_input(File, N, L),!,
  solve(L, N, [], Answer),
  ! .
