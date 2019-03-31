#include <iostream>
#include <algorithm>
#include <string>
#include <fstream>
#include <queue>

using namespace std;

struct Grid {
    int death_time = -1;
    int cat_time = -1;
    char symbol;
    string seq;
    bool visited = false;
    bool cat_visit = false;
};

Grid grid[1000][1000];

struct  cell {
  int row;
  int column;
  int t;
};

int main(int argc, char const *argv[]) {

  ifstream fin(argv[1]);

  queue<cell> q;
  queue<cell> cat;
  queue<cell> solutions;

  int N = 0, M = 0, maxM = 0;
  char a;
  while(fin.get(a)) {
      grid[N][M].symbol = a;
      if(a == '\n') {
        maxM = M;
        M = 0;
        N++;
      }
      else if(a == 'W') {
        cell c = {N, M, 0};
        q.push(c);
        grid[N][M].death_time = 0;
        grid[N][M].visited = true;
      }
      else if(a == 'X') {
        grid[N][M].symbol = a;
      }
      else if(a == 'A') {
        grid[N][M].symbol = '.';
        grid[N][M].cat_time = 0;
        grid[N][M].seq = "s";
        grid[N][M].cat_visit = true;
        cell c = {N, M, 0};
        cat.push(c);
      }
      M++;
  }


  M = maxM;
  int max_time = 0;

  while(q.size()) {

    struct cell a = q.front(); q.pop();
    max_time = a.t;

    if(a.row > 0) {
      Grid upCell = grid[a.row-1][a.column];
      if(upCell.symbol == '.' && !upCell.visited) {
        upCell.death_time = a.t + 1;
        upCell.visited = true;
        q.push(cell{a.row-1, a.column, a.t+1});
      }
    }

    if(a.row < N-1) {
      Grid downCell = grid[a.row+1][a.column];
      if (downCell.symbol == '.' && !downCell.visited) {
        downCell.death_time = a.t + 1;
        downCell.visited = true;
        q.push(cell{a.row+1, a.column, a.t+1});
      }
    }

    if(a.column > 0) {
      Grid leftCell = grid[a.row][a.column-1];
      if (leftCell.symbol=='.' && !leftCell.visited) {
        leftCell.death_time = a.t + 1;
        leftCell.visited = true;
        q.push(cell{a.row, a.column-1, a.t+1});
      }
    }

    if(a.column < M-1) {
      Grid rightCell = grid[a.row][a.column+1];
      if (rightCell.symbol == '.' && !rightCell.visited) {
        rightCell.death_time = a.t + 1;
        rightCell.visited = true;
        q.push(cell{a.row, a.column+1, a.t+1});
      }
    }
  }

  while(cat.size()) {

    struct cell a = cat.front(); cat.pop();
    Grid currentCell = grid[a.row][a.column];

    if (a.row < N-1) {
      Grid downCell = grid[a.row+1][a.column];
      if (downCell.symbol == '.' && !downCell.cat_visit) {
        downCell.cat_time = a.t + 1;
        downCell.cat_visit = true;
        downCell.seq = currentCell.seq + 'D';
        cat.push(cell{a.row+1, a.column, a.t+1});
      }
    }

    if (a.column > 0) {
      Grid leftCell = grid[a.row][a.column-1];
      if(leftCell.symbol == '.' && !leftCell.cat_visit) {
        leftCell].cat_time = a.t + 1;
        leftCell.cat_visit = true;
        leftCell.seq = currentCell.seq + 'L';
        cat.push(cell{a.row, a.column-1, a.t+1});
      }
    }

    if (a.column < M-1) {
      Grid rightCell = grid[a.row][a.column+1];
      if (rightCell.symbol == '.' && !rightCell.cat_visit) {
        rightCell.cat_time = a.t + 1;
        rightCell.cat_visit = true;
        rightCell.seq = currentCell.seq + 'R';
        cat.push(cell{a.row, a.column+1, a.t+1});
      }
    }

    if (a.row > 0) {
      Grid upCell = grid[a.row-1][a.column];
      if(upCell.symbol == '.' && !upCell.cat_visit) {
        upCell.cat_time = a.t + 1;
        upCell.cat_visit = true;
        upCell.seq = currentCell.seq + 'U';
        cat.push(cell{a.row-1, a.column, a.t+1});
      }
    }
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

  cell best_sol = solutions.front();

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
