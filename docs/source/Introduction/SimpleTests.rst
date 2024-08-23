Running Constellation
======================

This section will describe how to generate RTL for an example NoC configuration, and run a RTL simulation of the NoC under a simple test harness.

In this section, we will use pre-defined example configurationes

Test Configs
--------------------------

A large set of example configurations are described in `src/main/scala/test/Configs.scala <https://github.com/ucb-bar/constellation/blob/master/src/main/scala/test/Configs.scala>`_. These configs are representative of a wide set of Constellation's target design space.

These configs fall into four categories, delineated by the prefix of the Config name.

.. list-table::
   :widths: 25 30 70 50
   :header-rows: 1

   * - Prefix
     - ChiselTest Prefix
     - Generates...
     - Tests...
   * - ``TestConfig``
     - ``NoCTest``
     - - NoC verilog
       - chisel random test harness
     - Functional correctness
   * - ``TLTestConfig``
     - ``NoCTestTL``
     - - NoC verilog
       - TileLink protocol adapters
       - TileLink transport harness
     - Functional correctness for TL transport
   * - ``AXI4TestConfig``
     - ``NoCTestAXI4``
     - - NoC verilog
       - AXI4 protocol adapters
       - AXI4 transport harness
     - Functional correctness for AXI4 transport


Chipyard-Standalone Testing
---------------------------

The test configurations can also be run in Chipyard-Standalone mode.

.. code-block:: shell

   cd chipyard/sims/vcs
   make SUB_PROJECT=constellation BINARY=none CONFIG=TestConfig00 run-binary-debug

After running the above command, the generated verilog will be in ``generated-src``.


.. code-block:: shell

   cd generated-src
   cd constellation.test.TestHarness.TestConfig00
   cat constellation.test.TestHarness.TestConfig00.top.v

A visualization of the NoC can also be generated

.. code-block:: shell

   NOC_PATH=$(pwd)/constellation.test.TestHarness.TestConfig00.test.noc.
   cd ~/chipyard/generators/constellation/scripts
   ./vis.py $NOC_PATH
