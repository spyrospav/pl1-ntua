#include <iostream>
#include <algorithm>
#include <string>
#include <fstream>

using namespace std;

int seq[1000000];
int freq[100001];

int main(int argc, char const *argv[]) {

  int N,K;
  cin >> N >> K;

  for(int i=0; i<N; i++){
    cin >> seq[i];
    freq[seq[i]]++;
  }

  bool impossible=false;

  for(int i=1; i<=K; i++){
    if(!freq[i]){
      impossible=true;
      break;
    }
  }

  if (impossible){
    cout << 0 << endl;
  }

  else{

    int start=0,finish=N-1;

    while (start<=finish){
      if (freq[seq[start]]==1 && freq[seq[finish]]==1){
        break;
      }
      else{
        if (freq[seq[start]]>1){
          //cout << "start : " << start << endl;
          freq[seq[start]]--;
          start++;
        }
        if (freq[seq[finish]]>1){
          //cout << "finish : " << finish << endl;
          freq[seq[finish]]--;
          finish--;
        }
      }
    }

    cout << finish-start+1 << endl;
  }
  return 0;
}
