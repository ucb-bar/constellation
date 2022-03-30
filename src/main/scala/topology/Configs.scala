package constellation.topology

import scala.math.pow
import freechips.rocketchip.config.{Field, Parameters, Config}

import constellation.noc.{NoCKey}
import constellation.routing.{RoutingRelation}
import constellation.channel.{UserIngressParams, UserEgressParams}

class WithUnidirectionalLineTopology(
  nNodes: Int = 2
) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(
    topology = new UnidirectionalLine(nNodes),
  )
})

class WithBidirectionalLineTopology(
  nNodes: Int = 2,
) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(
    topology = new BidirectionalLine(nNodes),
    routingRelation = RoutingRelation.bidirectionalLine,
  )
})

class WithUnidirectionalTorus1DTopology(
  nNodes: Int = 2,
) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(
    topology = new UnidirectionalTorus1D(nNodes),
    routingRelation = RoutingRelation.unidirectionalTorus1DDateline(nNodes),
  )
})

class WithBidirectionalTorus1DTopology(
  nNodes: Int = 2,
  randomRoute: Boolean = false
) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(
    topology = new BidirectionalTorus1D(nNodes),
    routingRelation = if (randomRoute) {
      RoutingRelation.bidirectionalTorus1DRandom(nNodes)
    } else {
      RoutingRelation.bidirectionalTorus1DShortest(nNodes)
    },
  )
})

class WithButterflyTopology(
  kAry: Int = 2,
  nFly: Int = 2,
) extends Config((site, here, up) => {
  case NoCKey => {
    val height = pow(kAry,nFly-1).toInt
    up(NoCKey, site).copy(
      topology = new Butterfly(kAry, nFly),
      routingRelation = RoutingRelation.butterfly(kAry, nFly),
    )
  }
})

class WithMesh2DTopology(
  nX: Int = 3,
  nY: Int = 3,
  routingRelation: (Int, Int) => RoutingRelation = RoutingRelation.mesh2DDimensionOrdered(),
) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(
    topology = new Mesh2D(nX, nY),
    routingRelation = routingRelation(nX, nY),
  )
})

class WithUnidirectionalTorus2DTopology(
  nX: Int = 3,
  nY: Int = 3,
) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(
    topology = new UnidirectionalTorus2D(nX, nY),
    routingRelation = RoutingRelation.dimensionOrderedUnidirectionalTorus2DDateline(nX, nY),
  )
})


class WithBidirectionalTorus2DTopology(
  nX: Int = 3,
  nY: Int = 3,
) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(
    topology = new BidirectionalTorus2D(nX, nY),
    routingRelation = RoutingRelation.dimensionOrderedBidirectionalTorus2DDateline(nX, nY),
  )
})

