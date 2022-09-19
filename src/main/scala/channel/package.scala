package constellation

import freechips.rocketchip.diplomacy.{InwardNodeHandle, OutwardNodeHandle}

package object channel {
  type ChannelInwardNode = InwardNodeHandle[EmptyParams, ChannelParams, ChannelEdgeParams, Channel]
  type ChannelOutwardNode = OutwardNodeHandle[EmptyParams, ChannelParams, ChannelEdgeParams, Channel]
}
