import java.io.BufferedReader;
import java.io.FileReader;
//import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Queue;
import java.util.LinkedList;

class cell{
	
	int x;
	int y;
	int t;
	
	public cell(int a, int b, int c) {
		x = a;
		y = b;
		t = c;
	}
}

public class SaveTheCat {
	
	public static void main ( String [] args ) throws Exception {
		
		//Read from file
		FileReader reader = new FileReader(args[0]);
	    BufferedReader br = new BufferedReader(reader);
	    String line;
	    List<char []> gridd = new ArrayList<char []>();
	    int N = 0;
        int M = 0;
        
        while ((line = br.readLine()) != null) {
            if (N == 0) {
        	    M = (int)line.length();
            }
    	    gridd.add(line.toCharArray());
            N++;
        }
        br.close();
        
        //Initialize grid
        char[][] grid = gridd.toArray(new char[gridd.size()][]);
        int[][] death_time = new int[N][M];
        int[][] cat_time = new int[N][M];
        char[][] move = new char[N][M];
        
        Queue<cell> q = new LinkedList<cell>();
        Queue<cell> cat = new LinkedList<cell>();
        Queue<cell> solutions = new LinkedList<cell>();
        
        for (int i=0; i<N; i++) {
        	for (int j=0; j<M; j++) {
        		
        		//System.out.print(grid[i][j]);
        		
        		if (grid[i][j] == 'W') {
        			death_time[i][j] = 0;
        			cat_time[i][j] = -1;
        			q.add(new cell(i,j,0));
        		}
        		
        		else if (grid[i][j] == 'X') {
        			death_time[i][j] = -1;
        			cat_time[i][j] = -1;
        		}
        		
        		else if (grid[i][j] == 'A') {
        			death_time[i][j] = -1;
        			cat_time[i][j] = 0;
        			grid[i][j]='.';
        			move[i][j] = (char) 0;
        			cat.add(new cell(i,j,0));
        		}
        		else if (grid[i][j] == '.') {
        			death_time[i][j] = -1;
        			cat_time[i][j] = -1;
        		}
        	}
        	
        	//System.out.println("");
        
        }
        
        int maxtime = 0;
        
        while(q.size() > 0) {
        	
        	cell p = q.remove();
        	
        	int row = p.x;
        	int column = p.y;
        	int t = p.t;
        	//System.out.print(row);
        	//System.out.print(" ");
        	//System.out.print(column);
        	//System.out.print(" ");
        	//System.out.print(t);
        	//System.out.println("");
        	maxtime = t;
        	
        	if(row > 0 && grid[row-1][column]=='.' && death_time[row-1][column] == -1){
        	      death_time[row-1][column] = t+1;
        	      cell c = new cell(row-1,column,t+1);
        	      q.add(c);
        	}

        	if(row < N-1 && grid[row+1][column]=='.' && death_time[row+1][column] == -1){
      	      	death_time[row+1][column] = t+1;
      	      	cell c = new cell(row+1,column,t+1);
      	      	q.add(c);
        	}

        	if(column > 0 && grid[row][column-1]=='.' && death_time[row][column-1] == -1){
      	      	death_time[row][column-1] = t+1;
      	      	cell c = new cell(row,column-1,t+1);
      	      	q.add(c);
        	}

        	if(column < M-1 && grid[row][column+1]=='.' && death_time[row][column+1] == -1){
      	      	death_time[row][column+1] = t+1;
      	      	cell c = new cell(row,column+1,t+1);
      	      	q.add(c);
        	}
        	
        }
        
        //System.out.println("");
	
		while(cat.size() > 0){
			
			cell p = cat.remove();
			
			int row = p.x;
	    	int column = p.y;
	    	int t = p.t;
	    	//System.out.print(row);
	    	//System.out.print(" ");
	    	//System.out.print(column);
	    	//System.out.print(" ");
	    	//System.out.print(t);
	    	//System.out.println("");
	
		    if (row < N-1 && grid[row+1][column]=='.' && cat_time[row+1][column] ==-1){
		        if (death_time[row+1][column] > t+1 || death_time[row+1][column] ==-1){
		            cat_time[row+1][column] = t+1;
		            move[row+1][column] = 'D';
		            cell c = new cell(row+1,column,t+1);
		            cat.add(c);
		        }
		    }
	
		    if (column > 0 && grid[row][column-1]=='.' && cat_time[row][column-1] ==-1){
		        if (death_time[row][column-1] > t+1 || death_time[row][column-1] ==-1){
		            cat_time[row][column-1] = t+1;
		            move[row][column-1] = 'L';
		            cell c = new cell(row,column-1,t+1);
		            cat.add(c);
		        }
		    }

		    if (column < M-1 && grid[row][column+1]=='.' && cat_time[row][column+1] ==-1){
		        if (death_time[row][column+1] > t+1 || death_time[row][column+1] ==-1){
		            cat_time[row][column+1] = t+1;
		            move[row][column+1] = 'R';
		            cell c = new cell(row,column+1,t+1);
		            cat.add(c);
		        }
		    }
	
		    if (row > 0 && grid[row-1][column]=='.' && cat_time[row-1][column] ==-1){
		        if (death_time[row-1][column] > t+1 || death_time[row-1][column] ==-1){
		            cat_time[row-1][column] = t+1;
		            move[row-1][column] = 'U';
		            cell c = new cell(row-1,column,t+1);
		            cat.add(c);
		        }
	    }

	  }

	
	for(int i=0; i<N; i++){
	    for(int j=0; j<M; j++){
	      if(cat_time[i][j] >= 0){
	        if(cat_time[i][j] < death_time[i][j] && death_time[i][j] <= maxtime)
	          solutions.add(new cell(i,j,death_time[i][j]-1));
	        else if(death_time[i][j] == -1)
	          solutions.add(new cell(i,j,-1));
	      }
	    }
	  }

	  cell best_sol=solutions.remove();

	  while(solutions.size() > 0){
	      cell a = solutions.remove();
	      if(a.t == best_sol.t){
	          if(a.x < best_sol.x){
	              best_sol = a;
	          }
	          else if(a.x == best_sol.x){
	              if(a.y < best_sol.y){
	                  best_sol = a;
	              }
	          }
	      }
	      else if(a.t > best_sol.t){
	          best_sol=a;
	      }
	  }
	  
	  if(best_sol.t == -1){
	      System.out.println("infinity");
	  }
	  else{
	      System.out.println(best_sol.t);
	  }

	  if(move[best_sol.x][best_sol.y] == (char) 0){
	      System.out.println("stay");
	  }
	  
	  else{
		  String s = "";
		  cell b = best_sol;
		  while(true) {
			  if(move[b.x][b.y] == 'U') {
		        b = new cell(b.x+1,b.y,1);
		        s = s + "U";
			  }
		      else if(move[b.x][b.y] == 'D') {
		        b = new cell(b.x-1,b.y,1);
		        s = s + "D";
		      }
		      else if(move[b.x][b.y] == 'L') {
		        b = new cell(b.x,b.y+1,1);
		        s = s + "L";
		      }
		      else if(move[b.x][b.y] == 'R') {
		        b = new cell(b.x,b.y-1,1);
		        s = s + "R";
		      }
		      else if(move[b.x][b.y] == (char) 0)
			        break;
		  }
		  String result = new StringBuilder(s).reverse().toString();
	      System.out.println(result);
	  }
	}
}
