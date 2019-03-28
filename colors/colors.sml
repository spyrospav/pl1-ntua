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

(*create test case 1 !!!!!!*)
val p = parse "a1.txt";
val N = #1 p;
val K = #2 p;
val l = #3 p;
val rev_l = rev(l);
val length_l = length l;
(* Control.Print.printDepth := 20; best command for SML*)
fun createBM (k,a,s) =
     if k = 1 then M.insert(a,1,s)
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

val new_tree = updateBM (l,createBM(K,M.empty,0));

fun check_for_solution (a,k) =
    if k >0 then (
        if Option.valOf(M.find(a,k)) > 0 then check_for_solution (a,k-1)
        else false
        )
    else
        true;

val flag = check_for_solution (new_tree,K);

(*
fun solve (l, l_back, a, acc) =
    let
        val x = hd l
        val y = hd l_back
    in
        x+y
    end;
*)
