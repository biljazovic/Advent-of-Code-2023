#include <iostream>
#include <vector>
#include <algorithm>

#define FOR(i, n) for(int i = 0; i < n; i++) 

using namespace std;

vector<string> mat;
int n, m;
vector<vector<pair<int, int>>> graf;

int end_x;
bool vis[100000];
int maxi = 0;

void dfs(int x, int dist) {
  if (x == end_x) {
    if (dist > maxi) {
      printf("new max: %d\n", dist);
      maxi = dist;
    }
  }
  for (auto [sus, d] : graf[x]) {
    if (vis[sus]) continue;
    vis[sus] = true;
    dfs(sus, dist+d);
    vis[sus] = false;
  }
}

int main() {
  string line;

  while (cin >> line) {
    mat.push_back(line);
  }

  n = mat.size();
  m = mat[0].size();

  FOR(i, (n*m)) graf.emplace_back();

  int j_start = 0;
  while (mat[0][j_start] == '#') j_start++;

  int dx[] = { -1, 1, 0, 0  };
  int dy[] = {  0, 0, 1, -1 };
#ifdef PART1
  char dc[] = { '^', 'v', '>', '<' };
#endif

  FOR(i, n) FOR(j, m) {
    if (mat[i][j] == '#') continue;
    int x = i*m + j;
    FOR(s, 4) {
      int ii = i + dx[s];
      int jj = j + dy[s];
      int xx = ii*m + jj;
      if (ii < 0 || ii >= n || jj < 0 || jj >= m || mat[ii][jj] == '#') continue;
#ifdef PART1
      if (mat[ii][jj] == '.' || s == (find(dc, dc+4, mat[ii][jj]) - dc))
#endif
        graf[x].push_back({xx, 1});
    }
  }

#ifndef PART1
  while (true) {
    auto it = find_if(graf.begin(), graf.end(), [](const auto& p) { return p.size() == 2; });
    if (it == graf.end()) break;
    int cur = distance(graf.begin(), it);
    auto [x, dx] = (*it)[0];
    auto [y, dy] = (*it)[1];
    auto itx = find_if(graf[x].begin(), graf[x].end(), [&](const auto& p) { return p.first == cur; });
    auto ity = find_if(graf[y].begin(), graf[y].end(), [&](const auto& p) { return p.first == cur; });
    itx->first = y;
    itx->second += ity->second;
    ity->first = x;
    ity->second = itx->second;
    it->clear();
  }
#endif

  int j_end = 0;
  while (mat[n-1][j_end] == '#') j_end++;
  end_x = (n-1)*m + j_end;

  vis[j_start] = true;
  dfs(j_start, 0);

  return 0;
}
