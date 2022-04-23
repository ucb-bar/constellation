package constellation.noc

import constellation.routing.{RoutingRelation}
import freechips.rocketchip.config.{Field, Parameters, Config}

class WithNoParamValidation extends Config((site, here, up) => {
  case NoCKey => up(NoCKey).copy(skipValidationChecks=true)
})

class WithCtrl extends Config((site, here, up) => {
  case NoCKey => up(NoCKey).copy(hasCtrl = true)
})
