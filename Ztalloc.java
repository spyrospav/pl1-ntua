import java.util.Scanner; 
import java.io.File;

public class Ztalloc {
  // The main function.
  public static void main(String args[]) throws Exception {
	File file = new File(args[0]);
	Scanner scanner = new Scanner(file);
	int Q = scanner.nextInt();
	while(Q-- > 0) {
		int Lin = scanner.nextInt();
		int Rin = scanner.nextInt();
		int Lout = scanner.nextInt();
		int Rout = scanner.nextInt();
		  
		if (Lin >= Lout && Rin <= Rout) {
	    	System.out.print("EMPTY");
	    }
	    else {
			Solver solver = new BFSolver();
		    State initial = new ZtallocState(Lin, Rin, 'i', Lout, Rout, null);
		    State result = solver.solve(initial);
		    if (result == null) {
		      System.out.print("IMPOSSIBLE");
		    } 
		    else {
		      printSolution(result);
		    }
		 }
		System.out.println("");
	}
	scanner.close();
  }

  // A recursive function to print the states from the initial to the final.
  private static void printSolution(State s) {
    if (s.getPrevious() != null) {
      printSolution(s.getPrevious());
    }
    System.out.print(s);
  }
}