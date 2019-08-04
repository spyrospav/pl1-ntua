head([X|_],X).
tail([_|X],X).

/*
 * ?- read_input('c1.txt', N, K, C).
 * N = 10,
 * K = 3,
 * C = [1, 3, 1, 3, 1, 3, 3, 2, 2|...].
 */
read_input(File, N, K, C) :-
    open(File, read, Stream),
    read_line(Stream, [N, K]),
    read_line(Stream, C).

read_line(Stream, L) :-
    read_line_to_codes(Stream, Line),
    atom_codes(Atom, Line),
    atomic_list_concat(Atoms, ' ', Atom),
    maplist(atom_number, Atoms, L).
	
init_array(_,0,_).
init_array(X,N,Y) :-
	NN is N-1,
	init_array(X,NN,NL),
	put_assoc(N,NL,0,Y).
	
increase(Array,Key,Result) :-
	get_assoc(Key,Array,Value),
	NewValue is Value+1,
	put_assoc(Key,Array,NewValue,Result).
	
decrease(Array,Key,Result) :-
	get_assoc(Key,Array,Value),
	NewValue is Value-1,
	put_assoc(Key,Array,NewValue,Result).

/*update(X,[],Y) :-
	Y=X.
update(Array,List,Result) :-
	head(List,X),
	tail(List,Y),
	update(Array,Y,NewArray),
	increase(NewArray,X,Result).*/

help_solve_a(Count, K, Array, L1, J, L2, I, Ans, N, Answer) :-
	( Count < K -> 
	 tail(L1,X1),
	 head(X1,X),
	 get_assoc(X, Array, Value),
	 ( Value =:= 0 ->
	  Count2 is Count+1,
	  increase(Array,X,NewArray),
	  JJ is J+1,
	  help_solve_b(Count2, K, NewArray, X1, JJ, L2, I, Ans, N, Answer)
	 ;
	  increase(Array,X,NewArray),
	  JJ is J+1,
	  help_solve_b(Count, K, NewArray, X1, JJ, L2, I, Ans, N, Answer)
	 );
	  help_solve_b(Count, K, Array, L1, J, L2, I, Ans, N, Answer)).
	  
help_solve_b(Count, K, Array, L1, J, L2, I, Ans, N, Answer) :-
	( Count =:= K -> 
	 head(L2, X),
	 get_assoc(X, Array, Value),
	 ( Value > 1, I < J ->
	  decrease(Array,X,NewArray),
	  tail(L2,Y),
	  II is I+1,
	  help_solve_b(Count, K, NewArray, L1, J, Y, II, Ans, N, Answer)
	 ;
	  decrease(Array,X,NewArray),
	  tail(L2,Y),
	  II is I+1,
	  Count2 is Count-1,
	  Ans2 is min(Ans, J-I+1),
	  solve(NewArray, J, L1, N, K, Ans2, II, Y, Count2, Answer)
	 );
	  solve(Array, J, L1, N, K, Ans, I, L2, Count, Answer)).
	
solve(Array, J, L1, N, K, Ans, I, L2, Count, Answer) :-
	( J < N-1 -> help_solve_a(Count, K, Array, L1, J, L2, I, Ans, N, Answer);
	  Answer is Ans).
	
colors(X,Answer) :-
	read_input(X, N, K, C),
	empty_assoc(Array),
	init_array(Array,K,NewArray),
	!,
	head(C,Y),
	increase(NewArray,Y,NewArray2),
	Ans is N+1,
	solve(NewArray2, 0, C, N, K, Ans, 0, C, 1, Answer1),
	( Answer1 > N -> Answer is 0;
	  Answer is Answer1).