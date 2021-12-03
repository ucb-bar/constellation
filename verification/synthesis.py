from z3 import *
from verify import *

# prepare data
data = read_files(sys.argv[1:])
nedges = max(data, key=lambda x: x[1])[1]
com_sets = [set() for i in range(nedges)]
for datum in data:
    for i, elem in enumerate(datum[4]):
        com_sets[i].update(elem)
coms = [list(com_sets[i]) for i in range(nedges)]

# declare variables
edges = [Bool(f'x{i}') for i in range(nedges)]

# create a base problem where each input must connect to output in at least one way in each dependency graph
s = Solver()
for idx, datum in enumerate(data):
    dup_edges = [Bool(f'x{i}_{idx}') for i in range(nedges)]
    # an edge can be active only if it is selected
    for i in range(nedges):
        s.add(Implies(dup_edges[i], edges[i]))
    # an edge can be active only if at least one of its receivers is active or if it is an output
    for sender, receivers in enumerate(datum[4]):
        if receivers:
            s.add(Implies(dup_edges[sender], Or([dup_edges[i] for i in receivers])))
        elif sender not in datum[3]:
            s.add(Not(dup_edges[sender]))
    # all inputs are active
    s.add(And([dup_edges[i] for i in datum[2]]))

# subgraph synthesis
while True:
    # solve
    r = s.check()
    if r == unsat:
        print('deadlock free property failed')
        exit(1)
    # get the solution
    print('candidate subgraph generated')
    m = s.model()
    # verify
    v = Solver()
    # dup edges as many as data
    dup_edges = [[Bool(f'x{i}_{j}') for i in range(nedges)] for j in range(len(data))]
    # direct dependency
    for sender, receivers in enumerate(coms):
        if receivers:
            v.add(Implies(edges[sender], Or([edges[i] for i in receivers] + [dup_edges[j][sender] for j in range(len(data))])))
        else:
            v.add(Not(edges[sender]))
    # indirect dependency
    for j, datum in enumerate(data):
        for sender, receivers in enumerate(datum[4]):
            if receivers:
                v.add(Implies(dup_edges[j][sender], Or([edges[i] for i in receivers] + [dup_edges[j][i] for i in receivers])))
            else:
                v.add(Not(dup_edges[j][sender]))
    # unselected edges are inactive
    for edge in edges:
        if not m[edge]:
            v.add(Not(edge))
    # at least one of edges is active
    v.add(Or(edges))
    # verify
    r = v.check()
    if r == unsat:
        print('candidate subgraph verified')
        break
    # get the current core
    print('counterexample found')
    m = v.model()
    core = []
    for edge in edges:
        if m[edge]:
            core.append(edge)
    # get the minimum core (optional)
    # modify problem
    s.add(Not(And(core)))
    
print('done')
for edge in edges:
    if m[edge]:
        print(edge)
