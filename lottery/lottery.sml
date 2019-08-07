(*local*)

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
	
	(*fun query_trie X Trie acc = 
		let 
			val k = hd X;
			val subtries_ = getSubtries Trie;
			val y = findChild k subtries_;
			val key = getTrieKey y;
			val value = getTrieValue y;
		in
			if key = ~1 then 
		end
		*)
	
	fun query_tickets [X] Trie = 
			let
				val qq = explode(X);
				val q = tl qq;
				(*val (lottery_numbers, money) = query_trie q Trie 0*)
				val lottery_numbers = 12;
				val money = 13;
			in
					print( Int.toString(lottery_numbers) ^ " " ^ Int.toString(money) ^ "\n")
			end
		| query_tickets L Trie =
			let
				val qq = explode(hd L);
				val q = tl qq;
				(*val (lottery_numbers, money) = query_trie q Trie 0*)
				val lottery_numbers = 12;
				val money = 13;
			in
				(
					print( Int.toString(lottery_numbers) ^ " " ^ Int.toString(money) ^ "\n");
					query_tickets (tl L) Trie
				)
			end
(*in*)
    fun lottery file_name=
        let
            val p = parse file_name
            val K = #1 p
            val N = #2 p
            val Q = #3 p
            val l1 = #4 p
            val l2 = #5 p
			val rl1 = reverse_q l1 []
			val rl2 = reverse_q l2 []
			val lotteryTrie = build_trie rl2 (Node(#"\n",0,[]))
        in
			lotteryTrie
			(*query_tickets rl1 lotteryTrie*)
        end
	
(*end*)
