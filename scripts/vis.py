import networkx as nx
import os
import sys
import matplotlib.pyplot as plt

generated_src_dir = sys.argv[1]

def get_file(ext):
    return sys.argv[1] + ext

adjlist = get_file("noc.adjlist")

xys = get_file("noc.xy")


G = nx.read_adjlist(adjlist, create_using=nx.DiGraph())
xys = {n: (float(x), float(y)) for n, x, y in [l.split(' ') for l in open(xys).read().splitlines()]}

def toColor(n):
    if "i" in n:
        return "#aaffaa"
    if "e" in n:
        return "#ffaaaa"
    return "#aaaaff"
nx.draw_networkx_nodes(G, xys,
                       node_size=100,
                       node_color=[toColor(n) for n in list(G)]
                       )
nx.draw_networkx_labels(G, xys)

ax = plt.gca()
edge_counts = dict()
for e in G.edges:
    if (e[0] < e[1]):
        t = (e[0], e[1])
    else:
        t = (e[1], e[0])
    if t not in edge_counts:
        edge_counts[t] = 0
    edge_counts[t] += 1

edge_indices = {t: 0 for t in edge_counts}
for e in G.edges:
    if (e[0] < e[1]):
        t = (e[0], e[1])
        flipped = False
    else:
        t = (e[1], e[0])
        flipped = True
    c = edge_counts[t]
    i = edge_indices[t]
    offset = -0.2 + 0.4 * (i + 1.0) / (c + 1.0)
    if (flipped):
        offset = offset * -1
    print(t, i, c, offset)
    ax.annotate("",
                xy=xys[e[0]], xycoords='data',
                xytext=xys[e[1]], textcoords='data',
                arrowprops=dict(arrowstyle="->", color="0",
                                shrinkA=5, shrinkB=5,
                                patchA=None, patchB=None,
                                connectionstyle="arc3,rad=rrr".replace('rrr',str(offset)),
                                ),
                )
    edge_indices[t] += 1
plt.axis('off')

plt.show()

