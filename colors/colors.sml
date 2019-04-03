local

    fun min (a, b) = if a < b then a else b;

    fun parse file =
        let
            fun readInt input = Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)
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

    fun increase (a,b) =
        let
            val j = #2 (M.remove(a,b))
        in
            M.insert(a,b,j+1)
        end;

    fun decrease (a,b) =
        let
            val j = #2 (M.remove(a,b))
        in
            M.insert(a,b,j-1)
        end;

    fun check (a,x) = Option.valOf(M.find(a,x))

    fun help_solve_a (count, k, t, l1, j, l2, i, ans, n) =
        if count < k then(
            if check(t, hd (tl l1)) = 0 then help_solve_b(count+1, k, increase(t, hd (tl l1)), tl l1, j+1, l2, i, ans, n)
            else help_solve_b(count, k, increase(t, hd (tl l1)), tl l1, j+1, l2, i, ans, n)
        )
        else help_solve_b (count, k, t, l1, j, l2, i, ans, n)

    and help_solve_b (count, k, t, l1, j, l2, i, ans, n) =
        if count = k then (
            if (check(t,hd l2) > 1 andalso i<j) then help_solve_b(count, k, decrease(t, hd l2), l1, j, tl l2, i+1, ans, n)
            else solve(decrease(t,hd l2), j, l1, n, k, min (ans, j-i+1), i+1, tl l2, count-1)
            )
        else solve(t, j, l1, n, k, ans, i, l2, count)

    and solve (t,j,l1,n,k,ans,i,l2,count) =
        if j < n-1 then help_solve_a(count, k, t, l1, j, l2, i, ans, n)
        else ans

    fun solution (a,n) = if a <= n then a else 0

in
    fun colors file_name=
        let
            val p = parse file_name
            val N = #1 p
            val K = #2 p
            val l = #3 p
            val new_tree = increase(createBM(K,M.empty,0),hd l)
        in
            print ((Int.toString (solution(solve(new_tree, 0, l, N, K, N+1, 0, l, 1), N))) ^ "\n")
        end;
end
