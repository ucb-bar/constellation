Protocol Support
=====================

Constellation supports an interface to automatically wrap a multi-channel communication
protocol on top of the NoC. This section describes the abstract interface for layering
any potential protocol onto the NoC, as well as the implementations which provide
AXI-4 and TileLink compliant interconnects.

Abstract Protocol Interface
---------------------------

To layer a protocol on top of the NoC, an instance of a case class extending ``ProtocolParams``
must be constructed.

.. literalinclude:: ../../../src/main/scala/protocol/Protocol.scala
   :language: scala
   :start-after: BEGIN: ProtocolParams
   :end-before: END: ProtocolParams

The required fields of ``ProtocolParams`` must be provided as follows:

 - ``minPayloadWidth`` is the required minimum payload width for flits to transport this protocol. It is the minimum    payload width at the ingress and egress terminals.
 - ``ingressNodes`` is an ordered list of physical node destination for all ingress terminals
 - ``egressNodes`` is an ordered list of physical node sources for all egress terminals
 - ``nVirtualNetworks`` is the number of virtual subnets in this protocol, often this is the number of protocol channels.
 - ``vNetBlocking`` describes the blocking/nonblocking relationships between virtual subnetworks in this protocol
 - ``flows`` is a list of possible flows for the protocol
 - ``genIO`` returns the protocol-level IO for the entire interconnect
 - ``interface`` provides the interface for the NoC through ``terminals: NoCTerminalIO`` and the interface for the protocol through ``protocol: Data``. This function should instantiate the user-defined protocol adapters to connect the two interfaces.

Standalone Protocol NoC
------------------------
A NoC with protocol support can be elaborated in Chisel using the ``ProtocolNoC`` generator.
The ``ProtocolNoCParams`` case class parameterizes both the network (through ``nocParams``), as well
as the protocol interface (though a list of ``ProtocolParams``.

.. literalinclude:: ../../../src/main/scala/protocol/Protocol.scala
   :language: scala
   :start-after: BEGIN: ProtocolNoC
   :end-before: END: ProtocolNoC

.. Note:: Multiple protocols can be supported on a shared interconnect by passing multiple ``ProtocolParams`` to ``protocolParams``

.. Note:: Harnesses for testing standalone non-diplomatic protocol-capable NoCs are currently not provided. At the moment, Diplomatic integration is recommended.

TileLink Protocol Params
^^^^^^^^^^^^^^^^^^^^^^^^
The ``TileLinkProtocolParams`` case class describes an instance of ``ProtocolParams`` for TileLink-C.
``edgesIn`` and ``edgesOut`` are ordered list of inwards and outwards TileLink edges (from masters and slaves, respectively). ``edgeInNodes`` and ``edgeOutNodes`` map the masters and slaves to physical node indices.
The definition of the ``TLEdge`` parameter class can be found in Rocketchip.

.. literalinclude:: ../../../src/main/scala/protocol/Tilelink.scala
   :language: scala
   :start-after: BEGIN: TileLinkProtocolParams
   :end-before: END: TileLinkProtocolParams

.. Note:: TL-C is a superset of TL-UL and TL-UH, so this implementation supports all potential TileLink implementations.

AXI-4 Protocol Params
^^^^^^^^^^^^^^^^^^^^^^^^
The ``AXI4ProtocolParams`` case class describes an instance of ``ProtocolParams`` for AXI4.
``edgesIn`` and ``edgesOut`` are ordered list of inwards and outwards AXI-4 edges (from masters and slaves, respectively). ``edgeInNodes`` and ``edgeOutNodes`` map the masters and slaves to physical node indices.
The definition of the ``AXI4EdgeParameters`` class can be found in Rocketchip.

.. literalinclude:: ../../../src/main/scala/protocol/AXI4.scala
   :language: scala
   :start-after: BEGIN: AXI4ProtocolParams
   :end-before: END: AXI4ProtocolParams

Diplomatic Protocol NoC
-----------------------

The common approach for generating interconnects within a Chipyard/Rocketchip-based SoC is to use the Diplomatic wrapper for a protocol-capable interconnet. Diplomacy's parameter negotiation will automatically construct a ``ProtocolParams`` case class to construct the ``TileLinkProtocolParams`` or ``AXI4ProtocolParams`` case classes from the Diplomatic graph.

These modules can replace existing ``TLXbar()`` and ``AXI4Xbar()`` Diplomatic modules in any Chipyard/Rocketchip design.

 - ``nocParams: NoCParams`` describes the physical topology and routing relation of the network
 - ``nodeMappings: DiplmaticNetworkNodeMapping`` contains mappings from ``String`` to a ``Int`` to map Diplomatic edges to physical node identifiers

.. literalinclude:: ../../../src/main/scala/protocol/AXI4.scala
   :language: scala
   :start-after: BEGIN: AXI4NoCParams
   :end-before: END: AXI4NoCParams


.. literalinclude:: ../../../src/main/scala/protocol/Tilelink.scala
   :language: scala
   :start-after: BEGIN: TLNoCParams
   :end-before: END: TLNoCParams


.. literalinclude:: ../../../src/main/scala/protocol/Protocol.scala
   :language: scala
   :start-after: BEGIN: NodeMapping
   :end-before: END: NodeMapping
