Channel Specification
---------------------

``channelParamGen`` is a function which determines the channel parameters for each directed
channel in the network. For every edge dictated by the ``PhysicalTopology``, this function is called
to determine the channel parameters for that edge.

This function can return a different set of parameters for each edge.


.. literalinclude:: ../../../src/main/scala/channel/Parameters.scala
   :language: scala
   :start-after: BEGIN: ChannelParams
   :end-before: END: ChannelParams


Virtual Channel Parameters
^^^^^^^^^^^^^^^^^^^^^^^^^^

The ``virtualChannelParams`` field contains a list of ``UserVirtualChannelParams`` objects, where
the each element represents one virtual channel, and the ``UserVirtualChannelParams`` object holds
the number of buffer entries for that virtual channel.


Diplomatic Channel Generation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


Clock Crossings
^^^^^^^^^^^^^^^^

Speedup
^^^^^^^^^^^^^
blah blah blah

Currently unsupported. One day this will allow auto insertion of clock crossings between
routers of the network on different clock domains.

