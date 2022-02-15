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
python3 ../../generators/constellation/scripts/vis.py generated-src/constellation.TestHarness.TestConfig00/constellation.TestHarness.TestConfig00.noc.
```
