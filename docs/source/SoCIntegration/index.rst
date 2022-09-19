Chipyard SoC Integration
=========================

Constellation NoCs can replace any TileLink crossbar in a Chipyard/Rocketchip SoC. The most common use case is for generating the interconnect for the SystemBus or the MemoryBus.

Examples of NoC integration can be found in the ``NoCConfigs.scala`` file in Chipyard, in the ``MultiNoCConfig``.


Global Shared Interconnect
--------------------------
While the default approach for integrating NoCs generates an independent network for each logical interconnect (i.e. the SystemBus and MemoryBus are wholly independent strucutres), Constellation also supports a shared global interconnect that is still deadlock-free. Configs pursuing this style of integration should set the ``GlobalNoCParams`` field with ``constellation.soc.WithGlobalNoC``.

An example of a global shared NoC config is the ``SharedNoCConfig`` in ``NoCConfigs.scala`` in Chipyard.
