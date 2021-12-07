#!/usr/bin/env python3
from z3 import *

# find a path from an input to a non-output terminal or a loop
def find_not_or_loop(num_channels, inputs, outputs, coms):
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

# find a loop
def find_loop(num_channels, coms):
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

# find a loop in an extended graph assuming liveness property
def find_loop_eg_live(num_channels, coms, graphs, escape_channels):
    # declare variables
    channels = [Bool(str(i)) for i in range(num_channels)]
    # create a verification problem instance
    s = Solver()
    # duplicate channels for each graph
    dup_channels = [[Bool(f'{i}_{j}') for i in range(num_channels)] for j in range(len(graphs))]
    # direct dependency
    for sender, receivers in enumerate(coms):
        s.add(Implies(channels[sender], Or([channels[i] for i in receivers] + [dup_channels[j][sender] for j in range(len(graphs))])))
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
    s.add(Or([channels[i] for i in escape_channels]))
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

# find a loop in an extended graph
def find_loop_eg(num_channels, coms, graphs, escape_channels, max_hop):
    # declare variables
    channels = [Bool(str(i)) for i in range(num_channels)]
    # create a verification problem instance
    s = Solver()
    # duplicate channels for each graph
    dup_channels = [[[Bool(f'{i}_{j}_{k}') for k in range(max_hop)] for i in range(num_channels)] for j in range(len(graphs))]
    # direct dependency
    for sender, receivers in enumerate(coms):
        s.add(Implies(channels[sender], Or([channels[i] for i in receivers] + [dup_channels[j][sender][max_hop-1] for j in range(len(graphs))])))
    # indirect dependency
    for j, graph in enumerate(graphs):
        for sender, receivers in enumerate(graph[4]):
            if receivers:
                s.add(Implies(dup_channels[j][sender][0], Or([channels[i] for i in receivers])))
                for k in range(1, max_hop):
                    s.add(Implies(dup_channels[j][sender][k], Or([dup_channels[j][sender][k-1]] + [dup_channels[j][i][k-1] for i in receivers])))
                    s.add(Implies(dup_channels[j][sender][k-1], dup_channels[j][sender][k]))
            else:
                for k in range(max_hop):
                    s.add(Not(dup_channels[j][sender][k]))
    # non-escape channels are inactive
    for i in range(num_channels):
        if i not in escape_channels:
            s.add(Not(channels[i]))
    # at least one of escape channels is active
    s.add(Or([channels[i] for i in escape_channels]))
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

# find a valid escape
def find_escape(num_channels, coms, graphs, live, max_hop = 0):
    # declare variables
    channels = [Bool(str(i)) for i in range(num_channels)]
    # create a synthesis problem instance
    s = Solver()
    # each channel must directly connect to an escape channel in each graph
    for graph_id, graph in enumerate(graphs):
        for sender, receivers in enumerate(graph[4]):
            if receivers:
                s.add(Or([channels[i] for i in receivers]))
    # escape synthesis
    while True:
        # solve the synthesis problem
        r = s.check()
        # check the result
        if r == unsat:
            print('valid escape does not exist')
            return False
        # get the tentative escape channels
        m = s.model()
        escape_channels = []
        for i, channel in enumerate(channels):
            if m[channel]:
                escape_channels.append(i)
        # verify the escape channels
        if live:
            loop = find_loop_eg_live(num_channels, coms, graphs, escape_channels)
        else:
            loop = find_loop_eg(num_channels, coms, graphs, escape_channels, max_hop)
        # break if verified
        if not loop:
            break
        # modify problem to prevent the found loop
        s.add(Not(And([channels[i] for i in loop])))
    # return the valid escape channels
    return escape_channels

def get_max_hop(num_channels, inputs, outputs, coms):
    max_hop = 0
    for k in inputs:
        done = [False for i in range(num_channels)]
        done[k] = True
        queue = [k]
        hop = 0
        while queue:
            next_queue = []
            for i in queue:
                if not coms[i] and i not in outputs:
                    return -1
                for j in coms[i]:
                    if not done[j]:
                        done[j] = True
                        next_queue.append(j)
            queue = next_queue
            hop += 1
        if hop > max_hop:
            max_hop = hop
    return max_hop

if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("-a", help="prove liveness property for each graph", action="store_true")
    parser.add_argument("-b", help="prove deadlock-free property by searching a loop assuming liveness property", action="store_true")
    parser.add_argument("-c", type=str, help="prove deadlock-free property by verifying the given escape channels assuming liveness property")
    parser.add_argument("-d", help="prove deadlock-free property by searching a valid escape assuming liveness property", action="store_true")
    parser.add_argument("-e", type=str, help="prove deadlock-free property by verifying the given escape channels")
    parser.add_argument("-f", help="prove deadlock-free property by searching a valid escape", action="store_true")
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
            path = find_not_or_loop(*graph[1:])
            if path:
                print(f'liveness property failed in {graph[0]} with a path:')
                print(*path)
            else:
                print(f'liveness property verified for {graph[0]}')

    if args.b or args.c or args.d or args.e or args.f:
        num_channels = max(graphs, key=lambda x: x[1])[1]
        com_sets = [set() for i in range(num_channels)]
        input_set = set()
        output_set = set()
        for graph in graphs:
            for i in graph[2]:
                input_set.add(i)
            for i in graph[3]:
                output_set.add(i)
            for i, elem in enumerate(graph[4]):
                com_sets[i].update(elem)
        coms = [list(com_sets[i]) for i in range(num_channels)]
        inputs = list(input_set)
        outputs = list(output_set)

    if args.b:
        loop = find_not_or_loop(num_channels, inputs, outputs, coms)
        if loop:
            print('deadlock-free property failed with a loop:')
            print(*loop)
        else:
            print('deadlock-free property verified')

    if args.c:
        with open(args.c, 'r') as fp:
            escape_channels = list(map(lambda x: int(x), fp.readline().split()))
            loop = find_loop_eg_live(num_channels, coms, graphs, escape_channels)
            if loop:
                print('deadlock-free property failed with a loop:')
                print(*loop)
            else:
                print('deadlock-free property verified')

    if args.d:
        escape_channels = find_escape(num_channels, coms, graphs, True)
        if escape_channels:
            print('deadlock-free property verified with escape channels:')
            print(*escape_channels)
        else:
            print('deadlock-free property failed')

    if args.e or args.f:
        max_hop = 0
        for graph in graphs:
            hop = get_max_hop(*graph[1:])
            if hop < 0:
                print('packet unreachable')
                exit(0)
            if hop > max_hop:
                max_hop = hop

    if args.e:
        with open(args.e, 'r') as fp:
            escape_channels = list(map(lambda x: int(x), fp.readline().split()))
            loop = find_loop_eg(num_channels, coms, graphs, escape_channels, max_hop)
            if loop:
                print('deadlock-free property failed with a loop:')
                print(*loop)
            else:
                print('deadlock-free property verified')

    if args.f:
        escape_channels = find_escape(num_channels, coms, graphs, False, max_hop)
        if escape_channels:
            print('deadlock-free property verified with escape channels:')
            print(*escape_channels)
        else:
            print('deadlock-free property failed')
