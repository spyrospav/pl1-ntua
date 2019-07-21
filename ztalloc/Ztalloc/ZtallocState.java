import java.util.*;

/* A class implementing the state of Ztalloc
 * machine problem
 */
public class ZtallocState implements State {
  // The positions of the four players; false = west and true = east.
  private int L, R, Lout, Rout;
  private char move;
  // The previous state.
  private State previous;

  public ZtallocState(int a, int b, char m, int c, int d, State p) {
    L =a; R = b; move = m; Lout = c; Rout = d;
    previous = p;
  }

  @Override
  public boolean isFinal() {
    return L >= Lout && R <= Rout;
  }
  
  @Override
  public boolean isBad() {
    return R >= 1000000;
  }

  @Override
  public Collection<State> next() {
    Collection<State> states = new ArrayList<>();
    states.add(new ZtallocState(L/2, R/2, 'h', Lout, Rout, this));
    if (3*R+1 < 1000000)
    	states.add(new ZtallocState(3*L+1, 3*R+1, 't', Lout, Rout, this));
    return states;
  }

  @Override
  public State getPrevious() {
    return previous;
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder("");
    if (move != 'i')
    	sb.append(move);
    return sb.toString();
  }

  // Two states are equal if all four are on the same shore.
  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    ZtallocState other = (ZtallocState) o;
    return L == other.L && R == other.R;
  }

  // Hashing: consider only the positions of the four players.
  @Override
  public int hashCode() {
    return Objects.hash(L, R);
  }
}