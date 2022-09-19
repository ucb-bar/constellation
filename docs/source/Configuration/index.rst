NoC Configuration
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

A specification is constructed as an instance of a ``NoCParams`` case class. Examples of
specifications can be seen in `src/main/scala/test/Configs.scala <https://github.com/ucb-bar/constellation/blob/master/src/main/scala/test/Configs.scala>`_.

.. toctree::
   :maxdepth: 3

   PhysicalSpec
   FlowSpec
   RoutingSpec

