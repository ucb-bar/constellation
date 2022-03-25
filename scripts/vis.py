#!/usr/bin/python3

import networkx as nx
import os
import sys
import argparse
import matplotlib.animation
import matplotlib.pyplot as plt
from matplotlib import cm

parser = argparse.ArgumentParser()
parser.add_argument("nocpath", help="path to noc debug info. Path should end in .noc.")
parser.add_argument("--animate", help="path to output log file for animating noc diagram", default="")
args = parser.parse_args()

def get_file(ext):
    return args.nocpath + ext

adjlist = get_file("adjlist")
xys = get_file("xy")
edgeprops = get_file("edgeprops")


G = nx.read_adjlist(adjlist, create_using=nx.DiGraph())
xys = {n: (float(x), float(y)) for n, x, y in [l.split() for l in open(xys).read().splitlines()]}
edgeprops = {(e[0], e[1]): e[2:] for e in [l.split() for l in open(edgeprops).read().splitlines()]}

def toColor(n):
    if "i" in n:
        return "#aaffaa"
    if "e" in n:
        return "#ffaaaa"
    return "#aaaaff"




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
edge_offsets = dict()
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
    edge_offsets[e] = str(offset)
    edge_indices[t] += 1

if args.animate:
    trace = open(sys.argv[2]).read().splitlines()
    trace = list(filter(lambda x: "nocsample" in x, trace))
    trace = {(int(t), e0, e1): int(n) for _, t, e0, e1, n in [l.split() for l in trace]}
    timestamps = {0:{e:0 for e in G.edges}}
    for k, e0, e1 in trace:
        if k not in timestamps:
            timestamps[k] = dict()
        timestamps[k][(e0, e1)] = trace[(k, e0, e1)]
    sorted_tscs = sorted(timestamps.keys())

    for i, k in list(enumerate(sorted_tscs))[1:]:
        for e in G.edges:
            if (e[0], e[1]) not in timestamps[k]:
                timestamps[k][e] = timestamps[sorted_tscs[i-1]][e]
else:
    sorted_tscs = [0, 99999]


def getPercentage(tsc, prev_tsc, e0, e1):
    if args.animate:
        packets = timestamps[tsc][(e0, e1)] - timestamps[prev_tsc][(e0, e1)]
        delta = tsc - prev_tsc
        return packets / (tsc - prev_tsc)
    else:
        return 0.0


ax = plt.gca()
fig = plt.gcf()

multiplier = 1
def update(num):
    ax.clear()
    nx.draw_networkx_nodes(G, xys, node_size=100, node_color=[toColor(n) for n in list(G)])
    nx.draw_networkx_labels(G, xys)

    tsc = sorted_tscs[(num+1)*multiplier]
    prev_tsc = sorted_tscs[(num)*multiplier]
    for e in G.edges:
        props = edgeprops[e]
        cmap = plt.get_cmap("inferno")
        if "unused" in props:
            color = "#eeeeee"
        else:
            percent = getPercentage(tsc, prev_tsc, e[0], e[1])*3
            color = cmap(percent)
        ax.annotate("",
                    xy=xys[e[0]], xycoords='data',
                    xytext=xys[e[1]], textcoords='data',
                    arrowprops=dict(arrowstyle="<-", color=color,
                                    lw=3,
                                    shrinkA=5, shrinkB=5,
                                    patchA=None, patchB=None,
                                    connectionstyle="arc3,rad=rrr".replace('rrr',edge_offsets[e])
                                    ),
                    )
        edge_indices[t] += 1
    ax.set_title("{} to {}".format(prev_tsc, tsc))

if args.animate:
    ani = matplotlib.animation.FuncAnimation(fig, update, frames=int(len(sorted_tscs)/multiplier) - 1, interval=30, repeat=True)
else:
    update(0)
#ani.save('animation.html', writer='imagemagick', fps=30)

plt.show()
