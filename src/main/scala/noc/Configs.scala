package constellation.noc

import constellation.topology.{TerminalPlaneTopology}
import constellation.routing.{RoutingRelation}
import freechips.rocketchip.config.{Field, Parameters, Config}

class WithNoParamValidation extends Config((site, here, up) => {
  case NoCKey => up(NoCKey).copy(skipValidationChecks=true)
})


class WithTerminalPlane extends Config((site, here, up) => {
  case NoCKey => {
    val nNodes = up(NoCKey).topology.nNodes
    up(NoCKey, site).copy(
      topology = new TerminalPlaneTopology(up(NoCKey).topology),
      routingRelation = RoutingRelation.terminalPlane(up(NoCKey).routingRelation, nNodes),
      ingresses = up(NoCKey).ingresses.map(i => i.copy(destId = i.destId + nNodes)),
      egresses = up(NoCKey).egresses.map(i => i.copy(srcId = i.srcId + 2 * nNodes))
    )
  }
})
