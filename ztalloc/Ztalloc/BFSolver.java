import java.util.*;

/* A class that implements a solver that explores the search space
 * using breadth-first search (BFS).  This leads to a solution that
 * is optimal in the number of moves from the initial to the final
 * state.
 */
public class BFSolver implements Solver {
  @Override
  public State solve (State initial) {
    Set<State> seen = new HashSet<>();
    Queue<State> remaining = new ArrayDeque<>();
    remaining.add(initial);
    seen.add(initial);
    int state_count=0;
    while (!remaining.isEmpty()) {
      State s = remaining.remove();
      state_count+=1;
      if (state_count > 1000000) break;
      if (s.isFinal()) return s;
      for (State n : s.next())
        if (!seen.contains(n) && !n.isBad()){
          remaining.add(n);
          seen.add(n);
        }
    }
    return null;
  }
}
