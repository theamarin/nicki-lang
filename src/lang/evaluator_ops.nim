import strutils, identifiers


func raiseUnexpectedDtypeException*(dtype: string, operation: string) =
   raise newException(ValueError, "Unexpected dtype " & escape(dtype) & " for " & operation)

func checkDtypesMatch(dtypeA, dtypeB: DtypeBase, operation: string) =
   if dtypeA != dtypeB:
      raise newException(ValueError, "Dtypes " & escape($dtypeA) & " and " & escape(
         $dtypeB) & " differ for " & operation)

func negative*(x: Value): Value =
   const op = "negation"
   case x.base
   of tint: return Value(base: tint, valInt: -x.valInt)
   else: raiseUnexpectedDtypeException($x.base, op)

func logicalNot*(x: Value): Value =
   const op = "logicalNot"
   case x.base
   of tbool: return Value(base: tbool, valBool: not x.valBool)
   else: raiseUnexpectedDtypeException($x.base, op)

func `+`*(a, b: Value): Value =
   const op = "addition"
   checkDtypesMatch(a.base, b.base, op)
   case a.base
   of tint: return Value(base: tint, valInt: a.valInt + b.valInt)
   else: raiseUnexpectedDtypeException($a.base, op)

func `-`*(a, b: Value): Value =
   const op = "subtraction"
   checkDtypesMatch(a.base, b.base, op)
   case a.base
   of tint: return Value(base: tint, valInt: a.valInt - b.valInt)
   else: raiseUnexpectedDtypeException($a.base, op)

func `*`*(a, b: Value): Value =
   const op = "multiplication"
   checkDtypesMatch(a.base, b.base, op)
   case a.base
   of tint: return Value(base: tint, valInt: a.valInt * b.valInt)
   else: raiseUnexpectedDtypeException($a.base, op)

func `div`*(a, b: Value): Value =
   const op = "division"
   checkDtypesMatch(a.base, b.base, op)
   case a.base
   of tint: return Value(base: tint, valInt: a.valInt div b.valInt)
   else: raiseUnexpectedDtypeException($a.base, op)


func `==`*(a, b: Value): Value =
   const op = "equality"
   checkDtypesMatch(a.base, b.base, op)
   case a.base
   of tbool: return Value(base: tbool, valBool: a.valBool == b.valBool)
   of tint: return Value(base: tbool, valBool: a.valInt == b.valInt)
   else: raiseUnexpectedDtypeException($a.base, op)

func `!=`*(a, b: Value): Value =
   const op = "inequality"
   checkDtypesMatch(a.base, b.base, op)
   case a.base
   of tbool: return Value(base: tbool, valBool: a.valBool != b.valBool)
   of tint: return Value(base: tbool, valBool: a.valInt != b.valInt)
   else: raiseUnexpectedDtypeException($a.base, op)

func `>`*(a, b: Value): Value =
   const op = "greater than"
   checkDtypesMatch(a.base, b.base, op)
   case a.base
   of tbool: return Value(base: tbool, valBool: a.valBool > b.valBool)
   of tint: return Value(base: tbool, valBool: a.valInt > b.valInt)
   else: raiseUnexpectedDtypeException($a.base, op)

func `>=`*(a, b: Value): Value =
   const op = "greater equals"
   checkDtypesMatch(a.base, b.base, op)
   case a.base
   of tbool: return Value(base: tbool, valBool: a.valBool >= b.valBool)
   of tint: return Value(base: tbool, valBool: a.valInt >= b.valInt)
   else: raiseUnexpectedDtypeException($a.base, op)

func `<`*(a, b: Value): Value =
   const op = "less than"
   checkDtypesMatch(a.base, b.base, op)
   case a.base
   of tbool: return Value(base: tbool, valBool: a.valBool < b.valBool)
   of tint: return Value(base: tbool, valBool: a.valInt < b.valInt)
   else: raiseUnexpectedDtypeException($a.base, op)

func `<=`*(a, b: Value): Value =
   const op = "less equals"
   checkDtypesMatch(a.base, b.base, op)
   case a.base
   of tbool: return Value(base: tbool, valBool: a.valBool <= b.valBool)
   of tint: return Value(base: tbool, valBool: a.valInt <= b.valInt)
   else: raiseUnexpectedDtypeException($a.base, op)


func `and`*(a, b: Value): Value =
   const op = "logical and"
   checkDtypesMatch(a.base, b.base, op)
   case a.base
   of tbool: return Value(base: tbool, valBool: a.valBool and b.valBool)
   else: raiseUnexpectedDtypeException($a.base, op)

func `or`*(a, b: Value): Value =
   const op = "logical or"
   checkDtypesMatch(a.base, b.base, op)
   case a.base
   of tbool: return Value(base: tbool, valBool: a.valBool or b.valBool)
   else: raiseUnexpectedDtypeException($a.base, op)

func `xor`*(a, b: Value): Value =
   const op = "logical xor"
   checkDtypesMatch(a.base, b.base, op)
   case a.base
   of tbool: return Value(base: tbool, valBool: a.valBool xor b.valBool)
   else: raiseUnexpectedDtypeException($a.base, op)
