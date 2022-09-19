Routers
------------------

Constellation's router generators have several microarchitectural configuration knobs.
The standard router microarchitecture follows the standard design pattern. A brief
description of the operation of a router follows:

 1. The head flit of the packet queries the router's ``RouteComputer`` to determine the next set
    candidate virtual channels it may allocate. This is the **RC** stage.
 2. The head flit of the packet queries the router's ``VirtualChannelAllocator`` to allocate
    a virtual channel from the candidate set of next virtual channels, in the **VA** stage
 3. Flits ask the ``SwitchAllocator`` for access to the crossbar switch in the router in the
    **SA** stage. This stage also checks that the next virtual channel has an empty buffer slot
    to accomodate this flit.
 4. A flit traverses the crossbar switch in the **ST** stage.

Flow-control is credit based. Matching ``InputUnit`` and ``OutputUnit`` pairs exchange credits over
a separate narrow channel to indicate the availability of buffer entries. A flit departing a ``InputUnit``
sends a credit backwards to the ``OutputUnit`` on its source router.

.. Note:: Constellation treats terminal Ingress and Egress points as special instances
	  of ``InputUnits`` and ``OutputUnits``.

.. Figure:: ../diagrams/router.svg
	    :width: 600px

	    The standard router micro-architecture

The ``router`` field of the ``NoCParams`` case class returns per-router parameters, enabling
heterogeneous designs with different router configurations.


Pipelining
^^^^^^^^^^^^^^^^

The standard pipeline microarchitecture is a 4-hop router, with RC, VA, SA, and ST on separate stages.
Two flags exist in the base router generator to reduce the hop count.

 - ``combineRCVA`` performs route-compute and virtual-channel-allocation in the same cycle. Routers
   which implement near-trivial routing policies may benefit from this setting.
 - ``combineSAST`` performs switch allocation and switch traversal in the same cycle. This is useful
   for low-radix routers.

 .. |pipeline_base| image:: ../diagrams/pipeline_base.svg
    :scale: 100%


 .. |pipeline_fast| image:: ../diagrams/pipeline_fast.svg
    :scale: 100%

+----------------------------------------+--------------------------------------------+
| | ``combineRCVA = false``              | | ``combineRCVA = true``                   |
| | ``combineSAST = false``              | | ``combineSAST = true``                   |
+========================================+============================================+
| |pipeline_base|                        | |pipeline_fast|                            |
+----------------------------------------+--------------------------------------------+

In the base microarchitecture, stalls can occur due to a delay in reallocating an output virtual
channel to a new packet. In the left diagram below, observe that no flit traverses the
switch in cycle 4, due to the delay for the virtual channel to be freed in cycle 3.

When ``coupleSAVA`` is enabled, the freed virtual channel is immediately made available on the
same cycle. However ``coupleSAVA`` can introduce long combinational paths on high-radix
routers.

 .. |credit_stall| image:: ../diagrams/credit_stall.svg
    :scale: 100%


 .. |credit_stall_free| image:: ../diagrams/credit_stall_free.svg
    :scale: 100%

+----------------------------------------+--------------------------------------------+
| ``coupleSAVA = false``                 | ``coupleSAVA = true``                      |
+========================================+============================================+
| |credit_stall|                         | |credit_stall_free|                        |
+----------------------------------------+--------------------------------------------+

Payload Width
^^^^^^^^^^^^^^^^

A router can specify its internal ``payloadWidth``. When routers with different payload
widths are connected by a channel, Constellation will autogenerate width-adapters
on the channels if the widths are multiples of each other. 



 .. |router_widths| image:: ../diagrams/router_widths.svg
    :scale: 200%

+-------------------------------------------------------------------------+--------------------+
| .. code:: scala                                                         | |router_widths|    |
|                                                                         |                    |
|    NoCParams(                                                           |                    |
|      topology = Mesh2D(2, 2)                                            |                    |
|      routerParams = (i) => UserRouterParams(payloadWidth =              |                    |
|        if (i == 1 or i == 2) 128 else 64),                              |                    |
|    )                                                                    |                    |
|                                                                         |                    |
+-------------------------------------------------------------------------+--------------------+


Virtual Channel Allocator
^^^^^^^^^^^^^^^^^^^^^^^^^

The has of the Virtual Channel Allocator has significant implications on the resulting
performance of the NoC. Currently, there are two categories of allocators implemented

 - **Single** VC allocators allocate only a single VC per cycle. These are useful in networks
   where most packets are multi-flit, as only the head flit needs to query the allocator
 - **Multi** VC allocators attempt to allocate multiple VCs per cycle.

The following allocator implementations are provided.

 - ``PIMMultiVCAllocator`` implements parallel-iterative-matching for a separable allocator
 - ``ISLIPMultiVCAllocator`` implements the ISLIP policy for a separable allocator
 - ``RotatingSingleVCAllocator`` rotates across incoming requests
 - ``PrioritizingSingleVCAllocator`` prioritizes certain VCs over others, according to the
   priorities given by the routing relation


