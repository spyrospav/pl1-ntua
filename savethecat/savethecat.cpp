
#include <iostream>
#include <algorithm>
#include <string>
#include <fstream>
#include <queue>

using namespace std;

struct Grid{
    int death_time=-1;
    int cat_time=-1;
    char symbol;
    string seq;
    bool visited=false;
    bool cat_visit=false;
};

Grid grid[1000][1000];


struct cell{
  int row;
  int column;
  int t;
};

int main(int argc, char const *argv[]) {

  ifstream fin(argv[1]);

  queue<cell> q;
  queue<cell> cat;
  queue<cell> solutions;

  int N=0, M=0, maxM=0;
  char a;
  while(fin.get(a)){

      grid[N][M].symbol=a;
      if(a=='\n'){
        maxM=M;
        M=0;
        N++;
      }

      else{

        if(a=='W'){
          cell c={N,M,0};
          q.push(c);
          grid[N][M].death_time=0;
          grid[N][M].visited=true;
        }

        else if(a=='X')
          grid[N][M].symbol=a;

        else if(a=='A'){
          grid[N][M].symbol='.';
          grid[N][M].cat_time=0;
          grid[N][M].seq="s";
          grid[N][M].cat_visit=true;
          cell c={N,M,0};
          cat.push(c);
        }

        M++;
      }
  }

  M=maxM;

  int max_time=0;

  while(q.size()){

    struct cell a=q.front();
    max_time=a.t;

    if(a.row > 0 && grid[a.row-1][a.column].symbol=='.' && !grid[a.row-1][a.column].visited){
      grid[a.row-1][a.column].death_time=a.t+1;
      grid[a.row-1][a.column].visited=true;
      q.push(cell{a.row-1,a.column,a.t+1});
    }

    if(a.row < N-1 && grid[a.row+1][a.column].symbol=='.' && !grid[a.row+1][a.column].visited){
      grid[a.row+1][a.column].death_time=a.t+1;
      grid[a.row+1][a.column].visited=true;
      q.push(cell{a.row+1,a.column,a.t+1});
    }

    if(a.column > 0 && grid[a.row][a.column-1].symbol=='.' && !grid[a.row][a.column-1].visited){
      grid[a.row][a.column-1].death_time=a.t+1;
      grid[a.row][a.column-1].visited=true;
      q.push(cell{a.row,a.column-1,a.t+1});
    }

    if(a.column < M-1 && grid[a.row][a.column+1].symbol=='.' && !grid[a.row][a.column+1].visited){
      grid[a.row][a.column+1].death_time=a.t+1;
      grid[a.row][a.column+1].visited=true;
      q.push(cell{a.row,a.column+1,a.t+1});
    }

    q.pop();

  }

  while(cat.size()){

    struct cell a=cat.front();

    if (a.row < N-1 && grid[a.row+1][a.column].symbol=='.' && !grid[a.row+1][a.column].cat_visit){
      grid[a.row+1][a.column].cat_time=a.t+1;
      grid[a.row+1][a.column].cat_visit=true;
      grid[a.row+1][a.column].seq=grid[a.row][a.column].seq+'D';
      cat.push(cell{a.row+1,a.column,a.t+1});
    }

    if (a.column > 0 && grid[a.row][a.column-1].symbol=='.' && !grid[a.row][a.column-1].cat_visit){
      grid[a.row][a.column-1].cat_time=a.t+1;
      grid[a.row][a.column-1].cat_visit=true;
      grid[a.row][a.column-1].seq=grid[a.row][a.column].seq+'L';
      cat.push(cell{a.row,a.column-1,a.t+1});
    }

    if (a.column < M-1 && grid[a.row][a.column+1].symbol=='.' && !grid[a.row][a.column+1].cat_visit){
      grid[a.row][a.column+1].cat_time=a.t+1;
      grid[a.row][a.column+1].cat_visit=true;
      grid[a.row][a.column+1].seq=grid[a.row][a.column].seq+'R';
      cat.push(cell{a.row,a.column+1,a.t+1});
    }

    if (a.row > 0 && grid[a.row-1][a.column].symbol=='.' && !grid[a.row-1][a.column].cat_visit){
      grid[a.row-1][a.column].cat_time=a.t+1;
      grid[a.row-1][a.column].cat_visit=true;
      grid[a.row-1][a.column].seq=grid[a.row][a.column].seq+'U';
      cat.push(cell{a.row-1,a.column,a.t+1});
    }

    cat.pop();

  }

  for(int i=0; i<N; i++){
    for(int j=0; j<M; j++){
      if(grid[i][j].cat_time >= 0){
        if(grid[i][j].cat_time < grid[i][j].death_time && grid[i][j].death_time <= max_time)
          solutions.push(cell{i,j,grid[i][j].death_time-1});
        else if(grid[i][j].death_time==-1)
          solutions.push(cell{i,j,-1});
      }
    }
  }

  cell best_sol=solutions.front();

  solutions.pop();
  while(solutions.size()){
      cell a=solutions.front();
      if(a.t == best_sol.t){
          if(a.row < best_sol.row){
              best_sol = a;
          }
          else if(a.row == best_sol.row){
              if(a.column < best_sol.column){
                  best_sol = a;
              }
          }
      }
      else if(a.t > best_sol.t){
          best_sol=a;
      }
      solutions.pop();
  }

  if(best_sol.t == -1){
      cout << "infinity" << endl;
  }
  else{
      cout << best_sol.t << endl;
  }

  if(grid[best_sol.row][best_sol.column].seq == "s"){
      cout << "stay" << endl;
  }
  else{
      grid[best_sol.row][best_sol.column].seq.erase(0,1);
      cout << grid[best_sol.row][best_sol.column].seq << endl;
  }

  return 0;
}
