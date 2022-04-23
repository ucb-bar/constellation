package constellation.routing

import freechips.rocketchip.config.{Field, Parameters, Config}
import constellation.noc.{NoCKey}
import constellation.topology.{TerminalPlane}

/** Creates N non-blocking virtual networks. Non-blocking virtual networks can share the physical
 *  resources of the NoC. However, if one virtual network is blocked, the others are not.
 *
 * @param n number of non-blocking virtual sub-networks
 */
class WithNNonblockingVirtualNetworks(n: Int) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(
    routingRelation = new NonblockingVirtualSubnetworksRouting(up(NoCKey, site).routingRelation, n),
    nVirtualNetworks = n
  )
})

/** Creates N blocking virtual networks. blocking virtual networks can share all the
 *  resources of the NoC. This means that if one virtual network i is blocked,
 * all virtual networks j > i are blocked while virtual networks j < i are not.
 *
 * @param n number of blocking virtual sub-networks
 */
class WithNBlockingVirtualNetworks(n: Int, nDedicatedChannels: Int = 1) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(
    routingRelation = new BlockingVirtualSubnetworksRouting(up(NoCKey, site).routingRelation, n, nDedicatedChannels),
    nVirtualNetworks = n,
    vNetBlocking = (blocker: Int, blockee: Int) => blocker < blockee
  )
})

/** Creates N non-blocking virtual networks with sharing. Non-blocking virtual networks with sharing
 * share some number of virtual channels but have at least one dedicated virtual channel each. Sharing
 * allows for higher-throughput as compared to virtual networks without sharing, while each subnetwork's
 * dedicated channel(s) ensure that if one virtual network is blocked, the others remain unblocked.
 *
 * @param n number of non-blocking virtual sub-networks
 * @param nSharedChannels number of virtual channels to share between each virtual network
 */
class WithNNonblockingVirtualNetworksWithSharing(n: Int, nSharedChannels: Int = 1) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(
    routingRelation = new SharedNonblockingVirtualSubnetworksRouting(
      up(NoCKey, site).routingRelation, n, nSharedChannels),
    nVirtualNetworks = n
  )
})

class WithRoutingRelation(r: RoutingRelation) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(routingRelation = r)
})

class WithTerminalPlaneRouting extends Config((site, here, up) => {
  case NoCKey => {
    val base = up(NoCKey).topology.asInstanceOf[TerminalPlane].base
    up(NoCKey).copy(routingRelation = new TerminalPlaneRouting(up(NoCKey).routingRelation, base.nNodes))
  }
})
