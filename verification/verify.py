#!/usr/bin/env python3
from z3 import *

# prove liveness property for the given inputs and outputs
def prove_liveness_property(num_channels, inputs, outputs, coms):
    # declare variables
    channels = [Bool(str(i)) for i in range(num_channels)]
    # create a problem instance
    s = Solver()
    # a channel can be active only if at least one of its receivers is active or if it is a non-output terminal
    for sender, receivers in enumerate(coms):
        if receivers:
            s.add(Implies(channels[sender], Or([channels[i] for i in receivers])))
    # all outputs are inactive
    for output in outputs:
        s.add(Not(channels[output]))
    # at least one of inputs is active
    s.add(Or([channels[i] for i in inputs]))
    # solve the problem
    r = s.check()
    # return an unreachable path if exists
    if r == unsat:
        return None
    m = s.model()
    path = []
    for i, channel in enumerate(channels):
        if m[channel]:
            path.append(i)
    return path

# prove deadlock-free property by searching a loop
def prove_deadlock_free_property_by_loop(num_channels, coms):
    # declare variables
    channels = [Bool(str(i)) for i in range(num_channels)]
    # create a problem instance
    s = Solver()
    # a channel can be active only if at least one of its receivers is active
    for sender, receivers in enumerate(coms):
        if receivers:
            s.add(Implies(channels[sender], Or([channels[i] for i in receivers])))
        else:
            s.add(Not(channels[sender]))
    # at least one of channels is active
    s.add(Or(channels))
    # solve the problem
    r = s.check()
    # return a loop if exists
    if r == unsat:
        return None
    m = s.model()
    loop = []
    for i, channel in enumerate(channels):
        if m[channel]:
            loop.append(i)
    return loop

# prove deadlock-free property by verifying the given escape channels
def prove_deadlock_free_property_by_escape(num_channels, coms, graphs, escape_channels):
    # declare variables
    channels = [Bool(str(i)) for i in range(num_channels)]
    # create a verification problem instance
    s = Solver()
    # duplicate channels for each graph
    dup_channels = [[Bool(f'{i}_{j}') for i in range(num_channels)] for j in range(len(graphs))]
    # direct dependency
    for sender, receivers in enumerate(coms):
        if receivers:
            s.add(Implies(channels[sender], Or([channels[i] for i in receivers] + [dup_channels[j][sender] for j in range(len(graphs))])))
        else:
            s.add(Not(channels[sender]))
    # indirect dependency
    for j, graph in enumerate(graphs):
        for sender, receivers in enumerate(graph[4]):
            if receivers:
                s.add(Implies(dup_channels[j][sender], Or([channels[i] for i in receivers] + [dup_channels[j][i] for i in receivers])))
            else:
                s.add(Not(dup_channels[j][sender]))
    # non-escape channels are inactive
    for i in range(num_channels):
        if i not in escape_channels:
            s.add(Not(channels[i]))
    # at least one of channels is active
    s.add(Or(channels))
    # solve the verification problem
    r = s.check()
    # return a loop if exists
    if r == unsat:
        return None
    m = s.model()
    loop = []
    for i, channel in enumerate(channels):
        if m[channel]:
            loop.append(i)
    return loop

# prove deadlock-free property by searching a valid subrelation
def prove_deadlock_free_property_by_subrelation(num_channels, coms, graphs):
    # declare variables
    channels = [Bool(str(i)) for i in range(num_channels)]
    # create a synthesis problem instance
    s = Solver()
    # each channel must be active or connect to an active channel in each graph
    for graph_id, graph in enumerate(graphs):
        for sender, receivers in enumerate(graph[4]):
            if receivers:
                s.add(Or([channels[sender]] + [channels[i] for i in receivers]))
    # each input must connect to outputs in at least one way in each graph
    for graph_id, graph in enumerate(graphs):
        # duplicate channels for each graph
        dup_channels = [Bool(f'{i}_{graph_id}') for i in range(num_channels)]
        # a duplicated channel can be active only if the original one is active
        for i in range(num_channels):
            s.add(Implies(dup_channels[i], channels[i]))
        # a duplicated channel can be active only if at least one of its receivers is active or if it is an output
        for sender, receivers in enumerate(graph[4]):
            if receivers:
                s.add(Implies(dup_channels[sender], Or([dup_channels[i] for i in receivers])))
            elif sender not in graph[3]:
                s.add(Not(dup_channels[sender]))
        # all inputs are active
        s.add(And([dup_channels[i] for i in graph[2]]))
    # subrelation synthesis
    while True:
        # solve the synthesis problem
        r = s.check()
        # check the result
        if r == unsat:
            print('valid subrelation does not exist')
            return False
        # get the tentative escape channels
        m = s.model()
        escape_channels = []
        for i, channel in enumerate(channels):
            if m[channel]:
                escape_channels.append(i)
        # verify the escape channels
        loop = prove_deadlock_free_property_by_escape(num_channels, coms, graphs, escape_channels)
        # break if verified
        if not loop:
            break
        # modify problem to prevent the found loop
        s.add(Not(And([channels[i] for i in loop])))
    # return the valid escape channels
    return escape_channels

if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("-a", help="prove liveness property for each graph", action="store_true")
    parser.add_argument("-b", help="prove deadlock-free property by searching a loop", action="store_true")
    parser.add_argument("-c", type=str, help="prove deadlock-free property by verifying the given escape channels")
    parser.add_argument("-d", help="prove deadlock-free property by searching a valid subrelation", action="store_true")
    parser.add_argument("graph", nargs='+')
    args = parser.parse_args()
    
    def read_files(filenames):
        graphs = []
        for filename in filenames:
            with open(filename, 'r') as fp:
                num_channels = int(fp.readline())
                coms = [[] for i in range(num_channels)]
                inputs = list(map(lambda x: int(x), fp.readline().split()))
                outputs = list(map(lambda x: int(x), fp.readline().split()))
                for line in fp:
                    line = list(map(lambda x: int(x), line.split()))
                    if len(line) < 2:
                        continue
                    coms[line[0]] += line[1:]
                graphs.append([filename, num_channels, inputs, outputs, coms])
        return graphs
    graphs = read_files(args.graph)

    if args.a:
        for graph in graphs:
            path = prove_liveness_property(*graph[1:])
            if path:
                print(f'liveness property failed in {graph[0]} with a path:')
                print(*path)
            else:
                print(f'liveness property verified for {graph[0]}')

    if args.b or args.c or args.d:
        num_channels = max(graphs, key=lambda x: x[1])[1]
        com_sets = [set() for i in range(num_channels)]
        for graph in graphs:
            for i, elem in enumerate(graph[4]):
                com_sets[i].update(elem)
        coms = [list(com_sets[i]) for i in range(num_channels)]

    if args.b:
        loop = prove_deadlock_free_property_by_loop(num_channels, coms)
        if loop:
            print('deadlock-free property failed with a loop:')
            print(*loop)
        else:
            print('deadlock-free property verified')

    if args.c:
        with open(args.c, 'r') as fp:
            escape_channels = list(map(lambda x: int(x), fp.readline().split()))
            loop = prove_deadlock_free_property_by_escape(num_channels, coms, graphs, escape_channels)
            if loop:
                print('deadlock-free property failed with a loop:')
                print(*loop)
            else:
                print('deadlock-free property verified')

    if args.d:
        escape_channels =prove_deadlock_free_property_by_subrelation(num_channels, coms, graphs)
        if escape_channels:
            print('deadlock-free property verified with escape channels:')
            print(*escape_channels)
        else:
            print('deadlock-free property failed')
