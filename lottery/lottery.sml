(*Lottery problem in SML-NJ*)

(*Using TRIE implentation from https://github.com/cannam/sml-trie*)

local

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

in
    fun lottery file_name=
        let
            val p = parse file_name
            val K = #1 p
            val N = #2 p
            val Q = #3 p
            val l1 = #4 p
            val l2 = #5 p
        in
            (K,N,Q,l1,l2)
        end
end
