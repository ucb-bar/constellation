package constellation.routing

import freechips.rocketchip.config.{Field, Parameters, Config}
import constellation.{NoCKey}

class WithNNonblockingVirtualNetworks(n: Int) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(
    routingRelation = RoutingRelation.nonblockingVirtualSubnetworks(
      up(NoCKey, site).routingRelation, n),
    nVirtualNetworks = n
  )
})

class WithNBlockingVirtualNetworks(n: Int) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(
    routingRelation = RoutingRelation.blockingVirtualSubnetworks(
      up(NoCKey, site).routingRelation, n),
    nVirtualNetworks = n,
    vNetBlocking = (blocker: Int, blockee: Int) => blocker < blockee
  )
})

class WithNNonblockingVirtualNetworksWithSharing(n: Int, nSharedChannels: Int = 1) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(
    routingRelation = RoutingRelation.sharedNonblockingVirtualSubnetworks(
      up(NoCKey, site).routingRelation, n, nSharedChannels),
    nVirtualNetworks = n
  )
})

