package constellation.topology

import scala.math.pow
import freechips.rocketchip.config.{Field, Parameters, Config}

import constellation.{NoCKey}
import constellation.routing.{RoutingRelation}
import constellation.channel.{UserIngressParams, UserEgressParams}

class UnidirectionalLineConfig(
  nNodes: Int = 2,
  ingressNodes: Seq[Int] = Seq(0),
  egressNodes: Seq[Int] = Seq(1)
) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(
    topology = new UnidirectionalLine(nNodes),
    ingresses = ingressNodes.map(i => UserIngressParams(i, (0 until egressNodes.size).toSet)),
    egresses = egressNodes.map(i => UserEgressParams(i))
  )
})

class BidirectionalLineConfig(
  nNodes: Int = 2,
  ingressNodes: Seq[Int] = Seq(0, 1),
  egressNodes: Seq[Int] = Seq(0, 1),
) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(
    topology = new BidirectionalLine(nNodes),
    routingRelation = RoutingRelation.bidirectionalLine,
    ingresses = ingressNodes.map(i => UserIngressParams(i, (0 until egressNodes.size).toSet)),
    egresses = egressNodes.map(i => UserEgressParams(i))
  )
})

class UnidirectionalTorus1DConfig(
  nNodes: Int = 2,
  ingressNodes: Seq[Int] = Seq(0),
  egressNodes: Seq[Int] = Seq(1),
) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(
    topology = new UnidirectionalTorus1D(nNodes),
    routingRelation = RoutingRelation.unidirectionalTorus1DDateline(nNodes),
    ingresses = ingressNodes.map(i => UserIngressParams(i, (0 until egressNodes.size).toSet)),
    egresses = egressNodes.map(i => UserEgressParams(i))
  )
})

class BidirectionalTorus1DConfig(
  nNodes: Int = 2,
  ingressNodes: Seq[Int] = Seq(0),
  egressNodes: Seq[Int] = Seq(1),
  randomRoute: Boolean = false
) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(
    topology = new BidirectionalTorus1D(nNodes),
    routingRelation = if (randomRoute) {
      RoutingRelation.bidirectionalTorus1DRandom(nNodes)
    } else {
      RoutingRelation.bidirectionalTorus1DShortest(nNodes)
    },
    ingresses = ingressNodes.map(i => UserIngressParams(i, (0 until egressNodes.size).toSet)),
    egresses = egressNodes.map(i => UserEgressParams(i))
  )
})

class ButterflyConfig(
  kAry: Int = 2,
  nFly: Int = 2
) extends Config((site, here, up) => {
  case NoCKey => {
    val height = pow(kAry,nFly-1).toInt
    up(NoCKey, site).copy(
      topology = new Butterfly(kAry, nFly),
      routingRelation = RoutingRelation.butterfly(kAry, nFly),
      ingresses = ((0 until height) ++ (0 until height)).map(
        i => UserIngressParams(i, (0 until 2*height).toSet, 0)),
      egresses = ((0 until height) ++ (0 until height)).map(
        i => UserEgressParams(i + height*(nFly-1)))
    )
  }
})

class Mesh2DConfig(
  nX: Int = 3,
  nY: Int = 3,
  routingRelation: (Int, Int) => RoutingRelation = RoutingRelation.mesh2DDimensionOrdered()
) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(
    topology = new Mesh2D(nX, nY),
    routingRelation = routingRelation(nX, nY),
    ingresses = (0 until nX * nY).map(i => UserIngressParams(i, (0 until nX * nY).toSet, 0)),
    egresses = (0 until nX * nY).map(i => UserEgressParams(i))
  )
})

class UnidirectionalTorus2DConfig(
  nX: Int = 3,
  nY: Int = 3,
) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(
    topology = new UnidirectionalTorus2D(nX, nY),
    routingRelation = RoutingRelation.dimensionOrderedUnidirectionalTorus2DDateline(nX, nY),
    ingresses = (0 until nX * nY).map(i => UserIngressParams(i, (0 until nX * nY).toSet, 0)),
    egresses = (0 until nX * nY).map(i => UserEgressParams(i))
  )
})


class BidirectionalTorus2DConfig(
  nX: Int = 3,
  nY: Int = 3,
) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(
    topology = new BidirectionalTorus2D(nX, nY),
    routingRelation = RoutingRelation.dimensionOrderedBidirectionalTorus2DDateline(nX, nY),
    ingresses = (0 until nX * nY).map(i => UserIngressParams(i, (0 until nX * nY).toSet, 0)),
    egresses = (0 until nX * nY).map(i => UserEgressParams(i))
  )
})


