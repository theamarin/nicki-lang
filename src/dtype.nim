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

func raiseUnexpectedDtypeException(dtype: Dtype, operation: string) =
   raise newException(ValueError, "Unexpected dtype " & escape($dtype) & " for " & operation)

func checkDtypesMatch(dtypeA, dtypeB: Dtype, operation: string) =
   if dtypeA != dtypeB:
      raise newException(ValueError, "Dtypes " & escape($dtypeA) & " and " & escape(
         $dtypeB) & " differ for " & operation)

func negative*(x: Value): Value =
   const op = "negation"
   case x.dtype
   of tint: return Value(dtype: tint, valInt: -x.valInt)
   else: raiseUnexpectedDtypeException(x.dtype, op)

func logicalNot*(x: Value): Value =
   const op = "logicalNot"
   case x.dtype
   of tbool: return Value(dtype: tbool, valBool: not x.valBool)
   else: raiseUnexpectedDtypeException(x.dtype, op)

func `+`*(a, b: Value): Value =
   const op = "addition"
   checkDtypesMatch(a.dtype, b.dtype, op)
   case a.dtype
   of tint: return Value(dtype: tint, valInt: a.valInt + b.valInt)
   else: raiseUnexpectedDtypeException(a.dtype, op)

func `-`*(a, b: Value): Value =
   const op = "subtraction"
   checkDtypesMatch(a.dtype, b.dtype, op)
   case a.dtype
   of tint: return Value(dtype: tint, valInt: a.valInt - b.valInt)
   else: raiseUnexpectedDtypeException(a.dtype, op)

func `*`*(a, b: Value): Value =
   const op = "multiplication"
   checkDtypesMatch(a.dtype, b.dtype, op)
   case a.dtype
   of tint: return Value(dtype: tint, valInt: a.valInt * b.valInt)
   else: raiseUnexpectedDtypeException(a.dtype, op)

func `div`*(a, b: Value): Value =
   const op = "division"
   checkDtypesMatch(a.dtype, b.dtype, op)
   case a.dtype
   of tint: return Value(dtype: tint, valInt: a.valInt div b.valInt)
   else: raiseUnexpectedDtypeException(a.dtype, op)


func `==`*(a, b: Value): Value =
   const op = "equality"
   checkDtypesMatch(a.dtype, b.dtype, op)
   case a.dtype
   of tbool: return Value(dtype: tbool, valBool: a.valBool == b.valBool)
   of tint: return Value(dtype: tbool, valBool: a.valInt == b.valInt)
   else: raiseUnexpectedDtypeException(a.dtype, op)

func `!=`*(a, b: Value): Value =
   const op = "inequality"
   checkDtypesMatch(a.dtype, b.dtype, op)
   case a.dtype
   of tbool: return Value(dtype: tbool, valBool: a.valBool != b.valBool)
   of tint: return Value(dtype: tbool, valBool: a.valInt != b.valInt)
   else: raiseUnexpectedDtypeException(a.dtype, op)

func `>`*(a, b: Value): Value =
   const op = "greater than"
   checkDtypesMatch(a.dtype, b.dtype, op)
   case a.dtype
   of tbool: return Value(dtype: tbool, valBool: a.valBool > b.valBool)
   of tint: return Value(dtype: tbool, valBool: a.valInt > b.valInt)
   else: raiseUnexpectedDtypeException(a.dtype, op)

func `>=`*(a, b: Value): Value =
   const op = "greater equals"
   checkDtypesMatch(a.dtype, b.dtype, op)
   case a.dtype
   of tbool: return Value(dtype: tbool, valBool: a.valBool >= b.valBool)
   of tint: return Value(dtype: tbool, valBool: a.valInt >= b.valInt)
   else: raiseUnexpectedDtypeException(a.dtype, op)

func `<`*(a, b: Value): Value =
   const op = "less than"
   checkDtypesMatch(a.dtype, b.dtype, op)
   case a.dtype
   of tbool: return Value(dtype: tbool, valBool: a.valBool < b.valBool)
   of tint: return Value(dtype: tbool, valBool: a.valInt < b.valInt)
   else: raiseUnexpectedDtypeException(a.dtype, op)

func `<=`*(a, b: Value): Value =
   const op = "less equals"
   checkDtypesMatch(a.dtype, b.dtype, op)
   case a.dtype
   of tbool: return Value(dtype: tbool, valBool: a.valBool <= b.valBool)
   of tint: return Value(dtype: tbool, valBool: a.valInt <= b.valInt)
   else: raiseUnexpectedDtypeException(a.dtype, op)


func `and`*(a, b: Value): Value =
   const op = "logical and"
   checkDtypesMatch(a.dtype, b.dtype, op)
   case a.dtype
   of tbool: return Value(dtype: tbool, valBool: a.valBool and b.valBool)
   else: raiseUnexpectedDtypeException(a.dtype, op)

func `or`*(a, b: Value): Value =
   const op = "logical or"
   checkDtypesMatch(a.dtype, b.dtype, op)
   case a.dtype
   of tbool: return Value(dtype: tbool, valBool: a.valBool or b.valBool)
   else: raiseUnexpectedDtypeException(a.dtype, op)

func `xor`*(a, b: Value): Value =
   const op = "logical xor"
   checkDtypesMatch(a.dtype, b.dtype, op)
   case a.dtype
   of tbool: return Value(dtype: tbool, valBool: a.valBool xor b.valBool)
   else: raiseUnexpectedDtypeException(a.dtype, op)
