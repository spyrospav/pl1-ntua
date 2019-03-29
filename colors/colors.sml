local
    fun parse file =
        let
            fun readInt input =
                Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)

                val inStream = TextIO.openIn file

                val n = readInt inStream
                val k = readInt inStream
                val _ = TextIO.inputLine inStream

                fun readInts 0 acc = acc
                  | readInts i acc = readInts (i-1) (readInt inStream:: acc)
        in
            (n, k, rev(readInts n []))
        end;

    structure M = BinaryMapFn(
        struct
            type ord_key = int
            val compare = Int.compare
        end
    );

    fun createBM (k,a,s) =
        if k = 0 then a
        else createBM(k-1,M.insert(a,k,s),s);

    fun updateBM (l,a) =
        if l = [] then a
        else
            let
                val x = (#2 (M.remove(a,hd l)))+1
            in
                updateBM(tl l, M.insert(a,hd l,x))
            end;

    fun decrease (a,b) =
        let
            val j = #2 (M.remove(a,b))
        in
            M.insert(a,b,j-1)
        end;

    fun check_for_solution (a,k) =
        if k > 0 then (
            if Option.valOf(M.find(a,k)) > 0 then check_for_solution (a,k-1)
            else false
            )
        else
            true;

    fun solve (l,l_back,a,acc,flag) =
        if flag = true then(
            if Option.valOf(M.find(a, hd l)) = 1 then acc
            else solve (tl l, l_back, decrease(a,hd l), acc+1, true)
        )
        else(
            if Option.valOf(M.find(a, hd l)) = 1 then solve (l_back,tl l, a, acc+1, true)
            else solve (l_back, tl l, decrease(a, hd l), acc+1, false)
        );

    fun solution (l, a, possible, N) =
        if possible = true
            then N - solve (l, rev(l), a, 0, false) + 1
        else 0;
in
    fun colors file_name=
        let
            val p = parse file_name
            val N = #1 p
            val K = #2 p
            val l = #3 p
            val new_tree = updateBM (l,createBM(K,M.empty,0))
        in
            print ((Int.toString (solution(l,new_tree,check_for_solution(new_tree,K),N))) ^ "\n")
        end;
end
(* Control.Print.printDepth := 20; best command for SML*)
