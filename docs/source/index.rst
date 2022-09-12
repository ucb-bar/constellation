.. constellation documentation master file, created by
   sphinx-quickstart on Sun Aug 14 14:28:02 2022.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Welcome to Constellation's Documentation!
=========================================



Constellation is a Chisel NoC RTL generator framework designed from the ground up to support integration in a heterogeneous SoC and evaluation of highly irregular NoC architectures.

 - Constellation generates **packet-switched wormhole-routed networks with virtual networks and credit-based flow control**
 - Constellation supports **arbitrary directed graph network topologies**, including **irregular** and **hierarchical** network topologies
 - Constellation includes a **routing algorithm verifier and routing-table compiler**, which can verify and generate deadlock-free routing tables for arbitrary topologies
 - Constellation is a **protocol-independent transport layer**, yet is capable of compliant deadlock-free transport of protocols like **AXI-4** and **TileLink**
 - Constellation supports drop-in **integration in Chipyard/Rocketchip SoCs**
 - Constellation is **rigorously tested**, with almost 100 different tests across as many network configurations


.. image:: diagrams/bigsoc.svg
   :width: 600px
   
.. toctree::
   :maxdepth: 4
   :caption: Contents:

   Introduction/index
   Configuration/index
   Protocols/index
   SoCIntegration/index
   Evaluation/index

Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`
