package com.rojoma.json.v3.interpolation

import scala.quoted.*

extension (inline ctx: StringContext) transparent inline def j(inline pieces: =>Any*):Any =
  ${ com.rojoma.json.v3.`-impl`.interpolation.JsonInterpolatorImpl.j('ctx, 'pieces) }

extension (inline ctx: StringContext) transparent inline def json(inline pieces: =>Any*):Any =
  ${ com.rojoma.json.v3.`-impl`.interpolation.JsonInterpolatorImpl.j('ctx, 'pieces) }
