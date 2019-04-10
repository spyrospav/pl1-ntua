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
(* i, j, waterTime, catTime, catMove *)
type node = (int * int * int * int * string)

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
    (* i, j, waterTime, catTime, catMove *)
    val emptyCell = (i, j, ~1, ~1, "")
    val catCell = (i, j, ~1, 0, "s")
    val waterCell = (i, j, 0, ~1, "")
    val c  = hd row (* character to process *)
  in
     if c = #"\n" then
        tree
      else if c = #"." then
        processRow i (j+1) (M.insert (tree, (i, j), emptyCell)) (tl row)
      else if c = #"X" then
        processRow i (j+1) tree (tl row)
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

(* return true if cat can go to cell else false *)
fun isCatValid (parentTime, cell:node option) =
  if cell = NONE then false
  else
    let
    (* i, j, waterTime, catTime, catMove *)
      val SOME (_, _, waterTime, catTime, _) = cell
    in
      (catTime = ~1) andalso (((parentTime+1) < waterTime) orelse waterTime = ~1)
    end;

(* update neighbors for cat bfs *)
fun updateCatNeighbors (tree: node M.map) [] catTime  = tree
  | updateCatNeighbors (tree: node M.map) ((cell:node option, direction)::cells) catTime =
    let
    (* i, j, waterTime, catTime, catMove *)
      val SOME (i, j, waterTime, _, _) = cell
      val x = (i, j, waterTime, (catTime+1), direction)
      val _ = enqueue catQueue x
    in
      updateCatNeighbors (M.insert (tree, (i, j), x)) cells catTime
    end;

(* run bfs for the cat *)
fun catBFS tree =
  let
    val (i, j, _, catTime, catMove) = dequeue catQueue
    val upCell = (M.find (tree, (i-1, j)), "U")
    val downCell = (M.find (tree, (i+1, j)), "D")
    val rightCell = (M.find (tree, (i, j+1)), "R")
    val leftCell = (M.find (tree, (i, j-1)), "L")
    val cells = List.filter (fn (x,_) => isCatValid (catTime , x)) [downCell, leftCell, rightCell, upCell]

    val updatedTree = (updateCatNeighbors tree cells catTime)
  in
    if isEmpty catQueue then tree
    else catBFS updatedTree
  end;

fun isWaterValid (cell:node option) =
  if cell = NONE then false
  else
    let
      val SOME (_, _, waterTime, _, _) = cell
    in
      (waterTime = ~1)
    end;

(* update neighbors for water bfs *)
fun updateWaterNeighbors tree [] _ = tree
  | updateWaterNeighbors (tree:node M.map) ((cell:node option)::cells) waterTime =
    let
      val SOME (i, j, _, catTime, catMove) = cell
      val x = (i, j, (waterTime+1), catTime, catMove)
      val _ = enqueue waterQueue x
    in
      updateWaterNeighbors (M.insert (tree, (i, j), x)) cells waterTime
    end;

(* run bfs for the water *)
fun waterBFS tree =
  if isEmpty waterQueue then tree
  else
    let
    (* i, j, waterTime, catTime, catMove *)
      val (i, j, waterTime, _, _) = dequeue waterQueue
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
      (* i, j, waterTime, catTime, catMove *)
      val (i1, j1, waterTime1, catTime1, catMove1) = x
      val (i2, j2, waterTime2, catTime2, catMove2) = acc
    in
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
    end;

(* from final cell return the time we save the cat *)
fun getTime (_, _, waterTime, _, _) =
  if (waterTime = ~1) then "infinity"
  else Int.toString (waterTime-1)

(* find path *)
fun getPath (i, j, move:string) tree path =
  if move = "s" then
    if path = "" then "stay"
    else path
  else
    let
      val parent_i = if move = "D" then i-1
                else if move = "U" then i+1
                else i
      val parent_j = if move = "R" then j-1
                else if move = "L" then j+1
                else j
      val parent = M.find (tree, (parent_i, parent_j))
    in
      if parent = NONE then "fail"
      else
        let
          val SOME (_,_,_,_,parent_move) = parent
        in
          getPath (parent_i, parent_j, parent_move) tree (move ^ path)
        end
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
      val updatedTree = catBFS (waterBFS initTree)

      val dummyCell = (1042, 1042, ~42, ~42, "")
      (*get cells visited by cat *)
      val filteredTree = M.filter (fn x => (#4 x) <> ~1) updatedTree
      val ans = M.foldl findAnswerCell dummyCell filteredTree

      val path = getPath (#1 ans, #2 ans, #5 ans) updatedTree ""

    in
     print((getTime ans)  ^ "\n" ^ path ^ "\n")
    end;
end;
