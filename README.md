The Constellation NoC Generator
=======================================================

## Compile

`CONSTELLATION_CI=1 sbt compile`

## Test

`CONSTELLATION_CI=1 sbt "testOnly constellation.NoCTest00"`

`CONSTELLATION_CI=1 sbt "testOnly constellation.NoCTest01"`

etc.

You can also run all tests with

`CONSTELLATION_CI=1 sbt "testOnly constellation.NoCTest*"`

## Generating Topology Diagrams
A GUI is required to view the diagram.

To run the diagram generator, the NoC must be run as part of the [Chipyard](https://github.com/ucb-bar/chipyard):
Currently you must switch to the `saturnv` branch of chipyard

```
git clone https://github.com/ucb-bar/chipyard.git
cd chipyard
git fetch origin saturnv
git checkout saturnv
pip3 install networkx
```

Once Chipyard has been [set up](https://chipyard.readthedocs.io/en/latest/Chipyard-Basics/Initial-Repo-Setup.html), the following commands can be used to generate a topology diagram for `TestConfig00`:

```
cd sims/vcs
make SUB_PROJECT=constellation CONFIG=TestConfig00
python3 ../../generators/constellation/scripts/vis.py generated-src/constellation.test.TestHarness.TestConfig00/constellation.test.TestHarness.TestConfig00.noc.
```

## Benchmarking and Testing a NoC Config
First, follow the same setup instructions as Generating Topology Diagrams.

Once Chipyard has been [set up](https://chipyard.readthedocs.io/en/latest/Chipyard-Basics/Initial-Repo-Setup.html), the following command can be used to test and benchmark `TestConfig00`:

```
cd sims/vcs
make clean && make SUB_PROJECT=constellation CONFIG=TestConfig00 MODEL=TestHarness USE_FSDB=1 BINARY=none run-binary-debug EXTRA_SIM_FLAGS="+TRAFFIC_MATRIX=./test_traffic_mat"
```

Where `test_traffic_mat` is a traffic matrix following the format:

```
<flits per packet> <warmup cycles> <measurement cycles> <drain timeout>

<ingress id> <egress id> <flow rate>
. . .
<ingress id> <egress id> <flow rate>
```

An example `test_traffic mat` for `TestConfig00` is:

```
4 2000 3000 500
0 0 0.25
```

This config generates `4` flits per packet, with `2000` warmup cycles, `3000` measurement cycles, and a `500` cycle drain timeout. Packets are injected from ingress `0` to egress `0` with a rate of `0.25` (this averages to 1 flit per cycle).

The simulation outputs a csv `noc-flow-throughputs.csv` that lists the throughput for each flow. The value for throughput printed to stdout is the minimum throughput of all flows, since this flow limits the throughput of the overall traffic pattern.

Minimum/median/maximum packet latencies are printed in units of cycles. The simulation outputs a csv `noc-packet-latencies.csv` with the latency of each packet. To visualize packet latencies as a histogram, use the script `scripts/packet_histogram.py`, passing in the filepath of `noc-packet-latencies.csv` as a command-line argument. A GUI is required to view the histogram.
