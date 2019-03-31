#include <iostream>
#include <fstream>

using namespace std;

int main(int argc, char ** argv) {

  ifstream fin(argv[1]);

  int N, K;
  fin >> N >> K;
  int A[N];
  int count[K+1];

  for (int i=0; i<N; i++) fin >> A[i];
  for (int i=1; i<=K; i++) count[i] = 0;


  int i = 0; //left end of the window.
  int j = 0; //right end of the window.
  int cnt = 1; //counter of the different elements in window.
  count[A[0]] = 1;
  int ans = N+1;

  while (j < N-1) {
    if (cnt < K) { //move right end.
      if (count[A[++j]]++ == 0) cnt++;
    }
    if (cnt == K) { //move left end.
      while (count[A[i]] > 1 && i<j) count[A[i++]]--;
      ans = min(ans, j-i+1);
      count[A[i++]]--;
      cnt--;
    }
  }

  if (ans <= N) cout << ans << endl;
  else cout << 0 << endl;

  return 0;
}
