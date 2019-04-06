local

structure M = BinaryMapFn(
    struct
        type ord_key = (int * int)
        fun compare ((a, b), (c, d)) =
            if (a < c) then LESS
            else if (c < a) then GREATER
            else if (b < d) then LESS
            else if (d < b) then GREATER
            else EQUAL;
    end
);
(* i, j, symbol, waterVisited, catVisited, waterTime, catTime, catPath *)
type node = (int * int * char * bool * bool * int * int * string)

(* functions for queue *)
fun isEmpty Q = Queue.isEmpty Q;
fun enqueue Q x = Queue.enqueue (Q, x);
fun dequeue Q = Queue.dequeue Q;

val catQueue = Queue.mkQueue () : node Queue.queue;
val waterQueue = Queue.mkQueue () : node Queue.queue;

(* convert string option to string *)
fun optionToString NONE = "none"
  | optionToString (SOME string) = string;

fun parse file =
    let
        val inStream = TextIO.openIn file
        fun readString acc =
          let
            val line = optionToString (TextIO.inputLine inStream)
          in
            if line = "none" then acc
            else explode line :: (readString acc)
          end;

        val graph = readString []
        val n = length graph
        val m = (length (hd graph)) - 1
    in
        (n , m, graph)
    end;

(* process row: update tree, cat queue and water queue for a grid row *)
fun processRow i j (tree: node M.map) row =
  let
    (* i, j, symbol, waterVisited, catVisited, waterTime, catTime, catPath *)
    val emptyCell = (i, j, #".", false, false, ~1, ~1, "")
    val catCell = (i, j, #".", false, true, ~1, 0, "s")
    val waterCell = (i, j, #"W", true, false, 0, ~1, "")
    val barrierCell = (i, j, #"X", false, false, ~1, ~1, "")
    val c  = hd row (* character to process *)
  in
     if c = #"\n" then
        tree
      else if c = #"." then
        processRow i (j+1) (M.insert (tree, (i, j), emptyCell)) (tl row)
      else if c = #"X" then
        processRow i (j+1) (M.insert (tree, (i, j), barrierCell)) (tl row)
      else if c = #"W" then
        let
          val _ = enqueue waterQueue waterCell (* update water queue *)
        in
          processRow i (j+1) (M.insert (tree, (i, j), waterCell)) (tl row)
        end
      else
        let
          val _ = enqueue catQueue catCell (* update cat queue *)
        in
          processRow i (j+1) (M.insert (tree, (i, j), catCell)) (tl row)
        end
  end;


(* initialize tree, catQueue and waterQueue *)
fun bfsInit i grid (tree: node M.map) =
  if grid = [] then tree
  else bfsInit (i+1) (tl grid) (processRow i 1 tree (hd grid))

(* return true if a cell symbol #"." else false *)
fun isCatValid (cell:node option) =
  if cell = NONE then false
  else
    let
    (* i, j, symbol, waterVisited, catVisited, waterTime, catTime, catPath *)
      val SOME (_, _, symbol, _, catVisited, _, _, _) = cell
    in
      if symbol = #"." andalso catVisited = false then true
      else false
    end;

(* update neighbors for cat bfs *)
fun updateCatNeighbors (tree: node M.map) [] catTime catPath  = tree
  | updateCatNeighbors (tree: node M.map) ((cell:node option, direction)::cells) catTime catPath =
    let
    (* i, j, symbol, waterVisited, catVisited, waterTime, catTime, catPath *)
      val SOME (i, j, symbol, waterVisited, _, waterTime, _, _) = cell
      val x = (i, j, symbol, waterVisited, true, waterTime, (catTime+1), (catPath ^ direction))
      val _ = enqueue catQueue x
    in
      updateCatNeighbors (M.insert (tree, (i, j), x)) cells catTime catPath
    end;

(* run bfs for the cat *)
fun catBFS tree =
  let
    val (i, j, _, _, _, _, catTime, catPath) = dequeue catQueue
    val upCell = (M.find (tree, (i-1, j)), "U")
    val downCell = (M.find (tree, (i+1, j)), "D")
    val rightCell = (M.find (tree, (i, j+1)), "R")
    val leftCell = (M.find (tree, (i, j-1)), "L")
    val cells = List.filter (fn (x,_) => isCatValid x) [downCell, leftCell, rightCell, upCell]

    val updatedTree = (updateCatNeighbors tree cells catTime catPath)
  in
    if isEmpty catQueue then tree
    else catBFS updatedTree
  end;

(* return true if a cell symbol #"." and not waterVisited else false *)
fun isWaterValid (cell:node option) =
  if cell = NONE then false
  else
    let
    (* i, j, symbol, waterVisited, catVisited, waterTime, catTime, catPath *)
      val SOME (_, _, symbol, waterVisited, _, _, _, _) = cell
    in
      if symbol = #"." andalso waterVisited = false then true
      else false
    end;

(* update neighbors for water bfs *)
fun updateWaterNeighbors tree [] _ = tree
  | updateWaterNeighbors (tree:node M.map) ((cell:node option)::cells) waterTime =
    let
      val SOME (i, j, symbol, _, catVisited, _, catTime, catPath) = cell
      val x = (i, j, symbol, true, catVisited, (waterTime+1), catTime, catPath)
      val _ = enqueue waterQueue x
    in
      updateWaterNeighbors (M.insert (tree, (i, j), x)) cells waterTime
    end;

(* run bfs for the water *)
fun waterBFS tree =
  if isEmpty waterQueue then tree
  else
    let
    (* i, j, symbol, waterVisited, catVisited, waterTime, catTime, catPath *)
      val (i, j, _, _, _, waterTime, _, _) = dequeue waterQueue
      val upCell = M.find (tree, (i-1, j))
      val downCell = M.find (tree, (i+1, j))
      val rightCell = M.find (tree, (i, j+1))
      val leftCell = M.find (tree, (i, j-1))
      val cells = List.filter (fn x => isWaterValid x) [downCell, leftCell, rightCell, upCell]

      val updatedTree = (updateWaterNeighbors tree cells waterTime)
    in
      if isEmpty waterQueue then tree
      else waterBFS updatedTree
    end;

(* return true if cat can survive at the cell, else false*)
fun catSurvives catVisited catTime waterTime =
  if catVisited then
    if (catTime < waterTime) orelse (waterTime = ~1) then
      true
    else
      false
  else
    false;

(* return true if cell (a,b) is more up and left from cell (c,d) *)
fun upLeft a b c d =
  if (a < c) then true
  else if (a = c) then
    if (b < d) then true
    else false
  else
    false

(* find the cell from which we will save the cat *)
(* helper function to perform foldl on the tree *)
fun findAnswerCell ((acc:node), (x:node)) =
    let
      (* i, j, symbol, waterVisited, catVisited, waterTime, catTime, catPath *)
      val (i1, j1, symbol1, waterVisited1, catVisited1, waterTime1, catTime1, catPath1) = x
      val (i2, j2, symbol2, waterVisited2, catVisited2, waterTime2, catTime2, catPath2) = acc
    in
      if (catSurvives catVisited1 catTime1 waterTime1) then
        if waterTime1 = ~1 then
          if waterTime2 = ~1 then
            if (upLeft i1 j1 i2 j2) then x
            else acc
          else x
        else if waterTime2 = ~1 then acc
        else if (waterTime1 > waterTime2) then x
        else if (waterTime2 > waterTime1) then acc
        else if (upLeft i1 j1 i2 j2) then x
        else acc
      else
        acc
    end;

(* remove barrier cells from tree *)
fun filterHelper (_, _, symbol, _, _, _, _, _) =
  if symbol = #"X" then false
  else true

(* from final cell return the time we save the cat *)
fun getTime (_, _, _, _, _, waterTime, _, _) =
  if (waterTime = ~1) then "infinity"
  else Int.toString (waterTime-1)

(**)
fun getPath (_, _, _, _, _, _, _, path) =
  if path = "s" then "stay"
  else
    let
      val len = String.size path
    in
      (* remove the initial symbol s from path*)
      String.substring (path, 1, len-1)
    end;

in

fun savethecat file_name =
    let
      (* Read input *)
      val input = parse file_name
      val n = #1 input
      val m = #2 input
      val grid = #3 input

      val initTree = bfsInit 1 grid M.empty
      (* run bfs and update initTree *)
      val updatedTree = waterBFS (catBFS initTree)
      (* remove barrierCells from updatedTree *)
      val finalTree = M.filter filterHelper updatedTree

      val dummyCell = (42, 42, #"d", true, true, ~42, ~42, "")
      val finalCell = M.foldl findAnswerCell dummyCell finalTree
    in
      print ((getTime finalCell) ^ "\n" ^ (getPath finalCell) ^ "\n")
    end;
end;
