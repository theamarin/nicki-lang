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


func `==`*(a, b: Value): Value =
   if a.dtype != b.dtype:
      raise newException(ValueError, "Dtypes " & escape($a.dtype) & " and " & escape(
            $b.dtype) & " differ for equality")
   case a.dtype
   of tbool: return Value(dtype: tbool, valBool: a.valBool == b.valBool)
   of tint: return Value(dtype: tbool, valBool: a.valInt == b.valInt)
   else: raise newException(ValueError, "Unexpected dtype " & escape($a.dtype) & " for equality")

func `!=`*(a, b: Value): Value =
   if a.dtype != b.dtype:
      raise newException(ValueError, "Dtypes " & escape($a.dtype) & " and " & escape(
            $b.dtype) & " differ for inequality")
   case a.dtype
   of tbool: return Value(dtype: tbool, valBool: a.valBool != b.valBool)
   of tint: return Value(dtype: tbool, valBool: a.valInt != b.valInt)
   else: raise newException(ValueError, "Unexpected dtype " & escape($a.dtype) & " for inequality")

func `>`*(a, b: Value): Value =
   if a.dtype != b.dtype:
      raise newException(ValueError, "Dtypes " & escape($a.dtype) & " and " & escape(
            $b.dtype) & " differ for greater than")
   case a.dtype
   of tbool: return Value(dtype: tbool, valBool: a.valBool > b.valBool)
   of tint: return Value(dtype: tbool, valBool: a.valInt > b.valInt)
   else: raise newException(ValueError, "Unexpected dtype " & escape($a.dtype) & " for greater than")

func `>=`*(a, b: Value): Value =
   if a.dtype != b.dtype:
      raise newException(ValueError, "Dtypes " & escape($a.dtype) & " and " & escape(
            $b.dtype) & " differ for greater equals")
   case a.dtype
   of tbool: return Value(dtype: tbool, valBool: a.valBool >= b.valBool)
   of tint: return Value(dtype: tbool, valBool: a.valInt >= b.valInt)
   else: raise newException(ValueError, "Unexpected dtype " & escape($a.dtype) & " for greater equals")

func `<`*(a, b: Value): Value =
   if a.dtype != b.dtype:
      raise newException(ValueError, "Dtypes " & escape($a.dtype) & " and " & escape(
            $b.dtype) & " differ for less than")
   case a.dtype
   of tbool: return Value(dtype: tbool, valBool: a.valBool < b.valBool)
   of tint: return Value(dtype: tbool, valBool: a.valInt < b.valInt)
   else: raise newException(ValueError, "Unexpected dtype " & escape($a.dtype) & " for less than")

func `<=`*(a, b: Value): Value =
   if a.dtype != b.dtype:
      raise newException(ValueError, "Dtypes " & escape($a.dtype) & " and " & escape(
            $b.dtype) & " differ for less equals")
   case a.dtype
   of tbool: return Value(dtype: tbool, valBool: a.valBool <= b.valBool)
   of tint: return Value(dtype: tbool, valBool: a.valInt <= b.valInt)
   else: raise newException(ValueError, "Unexpected dtype " & escape($a.dtype) & " for less equals")


func `and`*(a, b: Value): Value =
   if a.dtype != b.dtype:
      raise newException(ValueError, "Dtypes " & escape($a.dtype) & " and " & escape(
            $b.dtype) & " differ for logical and")
   case a.dtype
   of tbool: return Value(dtype: tbool, valBool: a.valBool and b.valBool)
   else: raise newException(ValueError, "Unexpected dtype " & escape($a.dtype) & " for logical and")

func `or`*(a, b: Value): Value =
   if a.dtype != b.dtype:
      raise newException(ValueError, "Dtypes " & escape($a.dtype) & " and " & escape(
            $b.dtype) & " differ for logical or")
   case a.dtype
   of tbool: return Value(dtype: tbool, valBool: a.valBool or b.valBool)
   else: raise newException(ValueError, "Unexpected dtype " & escape($a.dtype) & " for logical or")
