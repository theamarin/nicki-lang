type
   Dtype* = enum
      tbool
      tint

   Value* = object
      case dtype*: Dtype
      of tbool: valBool*: bool
      of tint: valInt*: int
