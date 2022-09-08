Configuring Constellation
===========================

Constellation divides the NoC specification into three orthogonal concerns.

 * **Physical specification** describes the topology and microarchitecture of the NoC
 * **Flow specification** describes what flows the network might expect
 * **Routing specification** describes how flows traverse the physical resources of the NoC

The total specification of the NoC is captured in the `constellation.noc.NoCParams` case
class. The total class is depicted below. Please see the subsections for more details.

.. literalinclude:: ../../../src/main/scala/noc/Parameters.scala
   :language: scala
   :start-after: BEGIN: NoC Parameters
   :end-before: END: NoC Parameters
   
.. toctree::
   :maxdepth: 3

   PhysicalSpec
   FlowSpec
   RoutingSpec
   OtherSpec
