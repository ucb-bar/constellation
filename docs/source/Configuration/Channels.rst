Channels
---------------------

``channelParamGen`` is a function which determines the channel parameters for each directed
channel in the network. For every edge dictated by the ``PhysicalTopology``, this function is called
to determine the channel parameters for that edge.

This function can return a different set of parameters for each edge.


.. literalinclude:: ../../../src/main/scala/channel/Parameters.scala
   :language: scala
   :start-after: BEGIN: ChannelParams
   :end-before: END: ChannelParams


Virtual Channels
^^^^^^^^^^^^^^^^^^^^^^^^^^

The ``virtualChannelParams`` field contains a list of ``UserVirtualChannelParams`` objects, where
the each element represents one virtual channel, and the ``UserVirtualChannelParams`` object holds
the number of buffer entries for that virtual channel.


Channel Generation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Currently, the ``channelGen`` field is only used to specify adding additional pipeline buffers in
a channel.

For example, the following segment sets a two-deep buffer on the channel.

.. code:: scala

          channelGen = (u) => {
            implicit val p: Parameters = u
            ChannelBuffer(2) := _
          }


.. Note:: In the future more functionality can be added through this interface.


Speedup
^^^^^^^^^^^^^

``srcSpeedup`` indicates the number of flits that may **enter** a channel in a cycle.
For ``srcSpeedup`` > 1, the generator will effectively increase the input bandwidth of
the channel.

``destSpeedup`` indicates the number of flits that may **exit** a channel in a cycle.
Increasing this pressures the routing resources and switch of the destination router.

.. Note:: Setting ``srcSpeedup > destSpeedup`` is an unusual design point.


Clock Crossings
^^^^^^^^^^^^^^^^

Currently unsupported. One day this will allow auto insertion of clock crossings between
routers of the network on different clock domains.


