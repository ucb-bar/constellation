Evaluation Framework
==========================

Constellation includes a lightweight C++ evaluation framework, designed to support future advanced traffic models.
Currently, the framework supports measuring per-flow latency and bandwidth using two traffic models.

 - An injection-rate based model, with per-flow injection rates
 - A trace-driven model, which loads Netrace trace files

Running a Simple Evaluation
-----------------------------
Traffic evaluations are best run in Chipyard-standalone mode.

.. code-block:: shell

   cd chipyard/sims/vcs
   make SUB_PROJECT=constellation BINARY=none CONFIG=EvalTestConfig00 MODEL=EvalHarness run-binary-debug

A generated ``noceval.cfg`` file will be generated in the ``generated-src`` directory. Copy this file elsewhere and modify it to adjust the simulation parameters. The important fields of this config file are:

 - ``warmup``: Number of cycles to spend in the warmup phase, to bring the network to steady-state
 - ``measurement``: Number of cycles after warmup in which throughput is measured
 - ``drain``: Number of cycles after measurement to wait for the network to drain. If the network does not drain in this many cycles, an assertion is fired
 - ``flits_per_packet``: Packet size
 - ``required_XXX``: Required throughput, median latency, max latency. IF measurement exceeds these, an assertion fires.
 - ``netrace_enable``: Use Netrace trace file as traffic model
 - ``netrace_trace``: Path to Netrace trace file
 - ``netrace_region``: Netrace region to begin trace replay at
 - ``flow x y z``: Specifies injection rate ``z`` for flow from ingress index ``x`` to egress index ``y``

 After modifying a ``noceval.cfg`` flag, the simulation can be rerun with:

.. code-block:: shell

   cd chipyard/sims/vcs
   make SUB_PROJECT=constellation BINARY=none CONFIG=EvalTestConfig00 MODEL=EvalHarness run-binary-debug EXTRA_SIM_FLAGS="+eval_params=path/to/noceval.cfg"
