from z3 import *

# prove liveness property for the given sources (inputs) and destination (outputs).
def prove_liveness_property(nedges, inputs, outputs, coms):
    # declare variables
    edges = [Bool(f'x{i}') for i in range(nedges)]
    # create a smt problem
    s = Solver()
    # an edge can be active only if at least one of its receivers is active or if it is a terminal
    for sender, receivers in enumerate(coms):
        if not receivers:
            continue
        s.add(Implies(edges[sender], Or([edges[i] for i in receivers])))
    # all output edges are inactive
    for output in outputs:
        s.add(Not(edges[output]))
    # at least one of input edges is active
    s.add(Or([edges[i] for i in inputs]))
    # solve the problem
    r = s.check()
    # show result
    if r == unsat:
        return True
    else:
        print('unreachable path exists')
        m = s.model()
        for edge in edges:
            if m[edge]:
                print(edge)
        return False

# prove deadlock free property
def prove_deadlock_free_property(nedges, coms):
    # declare variables
    edges = [Bool(f'x{i}') for i in range(nedges)]
    # create a smt problem
    s = Solver()
    # an edge can be active only if at least one of its receivers is active or if it is a terminal
    for sender, receivers in enumerate(coms):
        if receivers:
            s.add(Implies(edges[sender], Or([edges[i] for i in receivers])))
        else:
            s.add(Not(edges[sender]))
    # at least one of edges is active
    s.add(Or(edges))
    # solve the problem
    r = s.check()
    # show result
    if r == unsat:
        return True
    else:
        print('deadlock exists')
        m = s.model()
        for edge in edges:
            if m[edge]:
                print(edge)
        return False

    
def read_files(filenames):
    data = []
    import sys
    for filename in filenames:
        with open(filename, 'r') as fp:
            nedges = int(fp.readline())
            coms = [[] for i in range(nedges)]
            inputs = list(map(lambda x: int(x), fp.readline().split()))
            outputs = list(map(lambda x: int(x), fp.readline().split()))
            for line in fp:
                line = list(map(lambda x: int(x), line.split()))
                if len(line) < 2:
                    continue
                coms[line[0]] += line[1:]
            data.append([filename, nedges, inputs, outputs, coms])
    return data

if __name__ == "__main__":
    data = read_files(sys.argv[1:])
    for datum in data:
        if not prove_liveness_property(*datum[1:]):
            print(f'liveness property failed in {datum[0]}')
            exit(1)
    print('liveness property holds')
    nedges = max(data, key=lambda x: x[1])[1]
    com_sets = [set() for i in range(nedges)]
    for datum in data:
        for i, elem in enumerate(datum[4]):
            com_sets[i].update(elem)
    coms = [list(com_sets[i]) for i in range(nedges)]
    if not prove_deadlock_free_property(nedges, coms):
        print('deadlock free property failed')
        exit(1)
    print('deadlock free property holds')
