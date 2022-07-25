import strutils

type
   Dtype* = enum
      terror
      tbool
      tint

   Value* = object
      case dtype*: Dtype
      of terror: discard
      of tbool: valBool*: bool
      of tint: valInt*: int


func negative*(x: Value): Value =
   case x.dtype
   of tint: return Value(dtype: tint, valInt: -x.valInt)
   else: raise newException(ValueError, "Unexpected dtype " & escape($x.dtype) & " for negation")

func logicalNot*(x: Value): Value =
   case x.dtype
   of tbool: return Value(dtype: tbool, valBool: not x.valBool)
   else: raise newException(ValueError, "Unexpected dtype " & escape($x.dtype) & " for logical not")

func `+`*(a, b: Value): Value =
   if a.dtype != b.dtype:
      raise newException(ValueError, "Dtypes " & escape($a.dtype) & " and " & escape(
            $b.dtype) & " differ for addition")
   case a.dtype
   of tint: return Value(dtype: tint, valInt: a.valInt + b.valInt)
   else: raise newException(ValueError, "Unexpected dtype " & escape($a.dtype) & " for addition")

func `-`*(a, b: Value): Value =
   if a.dtype != b.dtype:
      raise newException(ValueError, "Dtypes " & escape($a.dtype) & " and " & escape(
            $b.dtype) & " differ for subtraction")
   case a.dtype
   of tint: return Value(dtype: tint, valInt: a.valInt - b.valInt)
   else: raise newException(ValueError, "Unexpected dtype " & escape($a.dtype) & " for subtraction")

func `*`*(a, b: Value): Value =
   if a.dtype != b.dtype:
      raise newException(ValueError, "Dtypes " & escape($a.dtype) & " and " & escape(
            $b.dtype) & " differ for multiplication")
   case a.dtype
   of tint: return Value(dtype: tint, valInt: a.valInt + b.valInt)
   else: raise newException(ValueError, "Unexpected dtype " & escape($a.dtype) & " for multiplication")

func `div`*(a, b: Value): Value =
   if a.dtype != b.dtype:
      raise newException(ValueError, "Dtypes " & escape($a.dtype) & " and " & escape(
            $b.dtype) & " differ for division")
   case a.dtype
   of tint: return Value(dtype: tint, valInt: a.valInt - b.valInt)
   else: raise newException(ValueError, "Unexpected dtype " & escape($a.dtype) & " for division")
