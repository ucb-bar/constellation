package constellation

import freechips.rocketchip.diplomacy.{InwardNodeHandle, OutwardNodeHandle}

package object channel {
  type ChannelInwardNode = InwardNodeHandle[ChannelParams, ChannelParams, ChannelEdgeParams, Channel]
  type ChannelOutwardNode = OutwardNodeHandle[ChannelParams, ChannelParams, ChannelEdgeParams, Channel]
}
