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

(*val colors file =*)
    (*val p = parse file*)
val p = parse "a1.txt";
val N = #1 p;
val K = #2 p;
val l = #3 p;
val k = M.empty;

(*
list - reverse list
  i  -      j
fun where we check head list then lookup at M, if 1 somehow we return (maybe an acc to act as flag)
else remove from M and insert h[i]--, same with j
*)

(*fun createBM (l,a) =
    if l nil then a
    else
        let
            val j = hd l;
        in
            if M.lookup(a,j) then
            createBM(tl,M.insert(a,j,  );
        end
*)

val tree = createBM(l,k)
(*fun that does a[b]-- *)
fun g (a,b) =
    let
        val j = #2 (M.remove(a,b))
    in
        M.insert(a,b,j-1)
    end;
