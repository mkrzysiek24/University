#include <iostream>

using namespace std;

int main() { 
    ios_base::sync_with_stdio(0);
    cin.tie(0);
    cout.tie(0);

    int n, q;
    cin >> n >> q;

    pair<int, int> edges[n - 1];
    
    for(int i=0; i < n - 1; i++) {
        int parent;
        cin >> parent;
        edges[i] = make_pair(parent, i + 2);
    }

    sort(edges, edges + n - 1);

    int parents[n];

    for(int i=0; i< n; i++) {
        parents[i] = -1;
    }

    parents[0] = 0;

    for(int i=1; i < n - 1; i++) {
        if(edges[i].first != edges[i-1].first) {
            parents[edges[i].first - 1] = i;
        }
    }

    int stack[n];
    int pointer = 0;
    int time = 0;
    int vertex = 0;
    int index = 0;
    int visited[n], pre[n], post[n];
    
    for(int i= 0; i< n; i++) {
        stack[i] = 0;
        visited[i] = 0;
        pre[i] = 0;
        post[i] = 0;
    }

    stack[0] = 0;

    while(pointer != -1) {
        vertex = stack[pointer];
        if(visited[vertex]) {
            time++;
            post[vertex] = time;
            pointer--;
        }
        else {
            visited[vertex] = 1;
            time++;
            pre[vertex] = time;
            index = parents[vertex];
            if(index != -1) {
                do {
                pointer++;
                stack[pointer] = edges[index].second - 1;
                index++;
                } while(index < n - 1 && edges[index - 1].first == edges[index].first);
            }
            
        }
    }

    int mother, child;
    for(int i= 0; i < q; i++) {
        cin >> mother >> child; 
        if(pre[mother-1] < pre[child-1] && post[mother - 1] > post[child - 1]) {
            cout << "TAK \n";
        }
        else {
            cout << "NIE \n";
        }
    }
    
}
