Flow Specification
========================

The NoC's flow specification consits of two components.

.. literalinclude:: ../../../src/main/scala/noc/Parameters.scala
   :language: scala
   :start-after: Flow specification
   :end-before: Routing specification

Flows
--------------------
The ``flows`` field of ``NoCParams`` is a list of ``FlowParams`` case classes, where each flow
uniquely identifies its source ingress terminal, destination egress terminal, and the virtual
subnetwork identifier.

The ``flows`` parameter is motivated by the observation that the actually supported traffic
patterns in a NoC is a subset of all possible ingress-egress pairs. Constellation can optimize
the generated RTL for only the flows the network is expected to handle at runtime.

.. literalinclude:: ../../../src/main/scala/channel/Parameters.scala
   :language: scala
   :start-after: BEGIN: FlowParams
   :end-before: END: FlowParams


The ``ingressId`` and ``egressId`` fields index the specified ingresses and egresses of the
NoC. That is, the ingress/egress indices decouple the flow specification from the underlying
physical implementation of the network

The ``vNetId`` field can be used to specify a virtual subnetwork identifier to the flow.
Virtual subnetworks are used to delineate between different channels of a actual messaging
protocol, and is necessary for avoiding protocol-deadlock.

Virtual Subnetworks
---------------------
The ``vNetBlocking`` function indicates which virtual subnetworks must make forwards progress
while some other virtual subnetwork is blocked. If ``vNetBlocking(x, y) == true``, then packets
from subnetwork ``x`` must make forwards progress while packets of subnetwork ``y`` are stalled.
