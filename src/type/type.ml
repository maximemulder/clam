module Build    = Build
module Context  = Context
module Error    = Error
module Kind     = Kind
module System   = System
module Trans_ctx = Trans_ctx
module Trans_syn = Trans_syn
module Validate = Validate

exception Error = Error.Error

include Appear
include Compare
include Display
include Node
include Pol
include Rename
