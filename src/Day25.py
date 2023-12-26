f = open('res/input25')

import networkx as nx
G = nx.Graph()

for l in f.readlines():
    l = l.strip()
    s1 = l.split(': ')
    src = s1[0]
    dests = s1[1].split(' ')
    for dest in dests:
        G.add_edge(src, dest)

print(list(map(len, list(nx.k_edge_components(G, k = 4)))))
