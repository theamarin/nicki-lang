import tables, hashes, strutils
import diagnostics

type
   DtypeBase* = enum
      terror = "error",
      tvoid = "void",
      tbool = "bool",
      tint = "int",
      tstr = "str",
      tcomposed = "composed"

   ComposedDtypeKind* = enum
      ttype = "type", tfunc = "func", tstruct = "struct", tenum = "enum"

   ComposedDtype* = ref object
      name*: string
      pos*: Position
      case kind*: ComposedDtypeKind
      of ttype:
         dtype*: Dtype
      of tfunc:
         retDtype*: Dtype
         parameters*: seq[Identifier]
         hasImplementation*: bool
      of tstruct:
         members*: seq[Identifier]
      of tenum:
         enumerals*: OrderedTable[int, string]

   DtypeFlags* = enum
      dfConst, dfDiscardable

   Dtype* = ref object
      case base*: DtypeBase
      of terror, tvoid, tbool, tint, tstr: discard
      of tcomposed: composed*: ComposedDtype

   Identifier* = ref object
      name*: string
      pos*: Position
      dtype*: Dtype

   ComposedValue* = object
      case kind*: ComposedDtypeKind
      of ttype: dtype*: Dtype
      of tfunc: discard
      of tstruct: valStruct*: OrderedTable[Identifier, Value]
      of tenum: valEnum*: int

   Value* = ref object
      pos*: Position
      case base*: DtypeBase
      of terror, tvoid: discard
      of tbool: valBool*: bool
      of tint: valInt*: int
      of tstr: valStr*: string
      of tcomposed: composed*: ComposedValue


func hash*(self: Identifier): Hash =
   return cast[pointer](self).hash

func `==`*(l, r: Dtype): bool =
   if l.base != r.base: return false
   if l.base == tcomposed and l.composed != r.composed: return false
   return true

func isComposedType*(dtype: Dtype, kind: ComposedDtypeKind): bool =
   if dtype.base != tcomposed: return false
   return dtype.composed.kind == kind

func isComposedType*(identifier: Identifier, kind: ComposedDtypeKind): bool =
   return identifier.dtype.isComposedType(kind)

func `$`*(dtype: Dtype): string
func `$`*(dtype: ComposedDtype): string =
   assert not dtype.isNil, "composed dtype is nil"
   case dtype.kind
   of ttype:
      result = "type"
      if not dtype.dtype.isNil:
         result &= "<" & $dtype.dtype & ">"
   of tfunc:
      if not dtype.retDtype.isNil:
         result &= "("
         for idx, p in dtype.parameters:
            if idx > 0: result &= ", "
            result &= p.name & ": " & $p.dtype
         result &= "): " & $dtype.retDtype
         # if dtype.hasImplementation: result &= " = [implementation]"
   of tstruct:
      var s: seq[string]
      for member in dtype.members: s.add($member.dtype)
      result &= "struct<" & s.join(",") & ">"
   of tenum: result &= "enum"

func `$`*(dtype: Dtype): string =
   case dtype.base
   of terror, tvoid, tbool, tint, tstr: return $dtype.base
   of tcomposed: return $dtype.composed

func asTree*(dtype: Dtype): string = $dtype
func asCode*(dtype: Dtype): string = $dtype

func newDtype*(base: DtypeBase): Dtype = return Dtype(base: base)

func toValue*(dtype: Dtype): Value =
   if dtype.base == tcomposed:
      var composed = ComposedValue(kind: dtype.composed.kind)
      case dtype.composed.kind
      of ttype: composed.dtype = dtype.composed.dtype
      of tfunc: discard
      of tstruct:
         for member in dtype.composed.members:
            composed.valStruct[member] = member.dtype.toValue()
      of tenum: composed.valEnum = 0
      return Value(base: tcomposed, composed: composed)
   else:
      return Value(base: dtype.base)

func `$`*(id: Identifier): string =
   return id.name & ": " & $id.dtype

func asTree*(id: Identifier): string = $id
func asCode*(id: Identifier): string = id.name


func `$`*(val: Value): string
func `$`*(val: ComposedValue): string =
   case val.kind
   of ttype: return $val.dtype
   of tfunc: return "[func]"
   of tstruct:
      var res: seq[string]
      for identifier, value in val.valStruct:
         res.add(identifier.name & ": " & $value)
      return "{" & res.join(", ") & "}"
   of tenum: return "[enum]"

func `$`*(val: Value): string =
   case val.base
   of terror: return "[error]"
   of tvoid: return "[void]"
   of tbool: return $val.valBool
   of tint: return $val.valInt
   of tstr: return $val.valStr
   of tcomposed: return $val.composed

func asCode*(val: Value): string = $val
func asTree*(val: Value): string = $val
