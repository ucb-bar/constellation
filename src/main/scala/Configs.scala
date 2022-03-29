package constellation

import freechips.rocketchip.config.{Field, Parameters, Config}

class WithNoParamValidation extends Config((site, here, up) => {
  case NoCKey => up(NoCKey).copy(skipValidationChecks=true)
})
