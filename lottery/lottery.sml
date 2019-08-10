
fun  powm_help (_,0,c:IntInf.int,_) = c
	| powm_help (base:IntInf.int,exponent,c:IntInf.int,modulus) = 
		let
			val x = (c*base) mod modulus;
		in
			powm_help (base, (exponent-1), x, modulus)
		end;
		
fun powm (_,_,1) = 0
	| powm (base,exponent,modulus) = powm_help(LargeInt.fromInt base,exponent,LargeInt.fromInt 1,modulus);

fun helper [] = []
	| helper [x] = [x]
	| helper (l::ls) = (helper ls) @ [l];

fun reverse_string s = implode (helper (explode s));

fun optionToString NONE = "none"
  | optionToString (SOME string) = string;

fun parse file =
	let
		val inStream = TextIO.openIn file
		fun readInt input = Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)

		fun readString acc = optionToString (TextIO.inputLine inStream)

		val K = readInt inStream
		val N = readInt inStream
		val Q = readInt inStream
		val _ = TextIO.inputLine inStream

		fun readInts 0 acc = acc
		  | readInts i acc = readInts (i-1) (readString inStream :: acc)

	in
		(K ,N ,Q, rev(readInts N []), rev(readInts Q []))
	end;
	
fun reverse_q [] acc = acc
	| reverse_q L acc = 
		let
			val LL = tl L
			val s = reverse_string(hd L)
			val new_acc = s::acc
		in
			reverse_q LL new_acc
		end
		
datatype trie = Node of char * int * trie list;

fun getTrieKey (Node(Key,_,_)) = Key;
fun getTrieValue (Node(_,Value,_)) = Value;
fun getSubtries (Node(_,_,SubTries)) = SubTries;

fun delete (item, list) = List.filter(fn x => x <> item) list;
		
fun findChild _ [] = Node(#"x",~1,[])
	| findChild x SubTries = 
		if x = getTrieKey(hd SubTries) then hd SubTries
		else findChild x (tl SubTries);

fun build_new_trie [x] = (Node(x,1,[]))
	| build_new_trie L = (Node((hd L),1,[build_new_trie(tl L)]));

fun trie_insert [] acc = acc 
	| trie_insert L (Node(Key,Value,SubTries)) = 
		let
			val x = findChild (hd L) SubTries
			val key = getTrieKey x
			val value = getTrieValue x
			val subtries_ = getSubtries x
			
			fun replaceSubTrie node = delete(x,SubTries)
		in
			if key = #"x" then (Node(Key, Value, (build_new_trie L) :: SubTries))
			else (Node(Key, Value, (trie_insert (tl L) (Node(key, value+1,subtries_))) :: (replaceSubTrie(x))))
		end

fun build_trie [] acc = acc
	| build_trie Q acc = 
		let
			val qq = explode(hd Q);
			val q = tl qq;
		in
			build_trie (tl Q) (trie_insert q acc)
		end

fun query_trie _ (Node(_,V,[])) Depth = (((LargeInt.fromInt V) mod 1000000007) * (powm(2,Depth,1000000007)-1) mod 1000000007) mod 1000000007
	| query_trie X (Node(_,_,SubTries)) 0 = 
	let
		val k = hd X;
		val y = findChild k SubTries;
		val key = getTrieKey y;
	in
		if key = #"x" then LargeInt.fromInt 0
		else query_trie (tl X) y 1
	end
	| query_trie X (Node(_,V,SubTries)) Depth = 
		let 
			val k = hd X;
			val y = findChild k SubTries;
			val key = getTrieKey y;
			val value = getTrieValue y;
			val Power = (powm(2,Depth,1000000007)-1)  mod 1000000007;
			val VV = LargeInt.fromInt (V-value);
		in
			if key = #"x" then  ( (LargeInt.fromInt V) mod 1000000007 * Power) mod 1000000007
			else (((((VV mod 1000000007)*Power) mod 1000000007) mod 1000000007) + (((query_trie (tl X) y (Depth+1)) mod 1000000007 )) )mod 1000000007
		end


fun getNum C (Node(_,_,SubTries)) = 
	let
		val y = findChild C SubTries;
		val key = getTrieKey y;
	in
		if key = #"x" then 0
		else getTrieValue y
	end

fun query_tickets [X] Trie = 
		let
			val qq = explode(X);
			val q = tl qq;
			val lottery_numbers = getNum (hd q) Trie;
			val money = Int.fromLarge (query_trie q Trie 0);
		in
				print( (Int.toString lottery_numbers) ^ " " ^ (Int.toString money) ^ "\n")
		end
	| query_tickets L Trie =
		let
			val qq = explode(hd L);
			val q = tl qq;
			val lottery_numbers = getNum (hd q) Trie;
			val money = Int.fromLarge (query_trie q Trie 0);
		in
			(
				query_tickets (tl L) Trie;
				print( (Int.toString lottery_numbers) ^ " " ^ (Int.toString money) ^ "\n")
			)
		end
		


fun lottery file_name =
	let
		val p = parse file_name
		val K = #1 p
		val N = #2 p
		val Q = #3 p
		val l1 = #4 p
		val l2 = #5 p
		val rl1 = reverse_q l1 []
		val rl2 = reverse_q l2 []
		val lotteryTrie = build_trie rl1 (Node(#"\n",0,[]))
	in
		(*K,N,Q,l1,l2,rl1,rl2,lotteryTrie*)
		query_tickets (rl2) (lotteryTrie)
		
	end