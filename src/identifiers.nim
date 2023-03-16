import tables
import diagnostics

type
   DtypeBase* = enum
      # Error type (on binding/inference error)
      terror = "[error]"
      # Built-in data types
      tvoid = "void"
      tbool = "bool"
      tint = "int"
      tstr = "str"
      # Derived types (need more info)
      tfunc = "func"
      tstruct = "struct"
      tenum = "enum"

const
   basicDtypes* = {terror, tvoid, tbool, tint, tstr}
   basicDtypeStrings* = [("void", tvoid), ("bool", tbool), ("int", tint), ("str", tstr)].toTable

type
   Dtype* = ref object
      pos*: Position
      case base*: DtypeBase
      of basicDtypes: discard
      of tfunc:
         retDtype*: Dtype
         parameters*: seq[Parameter]
      of tstruct:
         members*: OrderedTable[string, Dtype]
      of tenum:
         enumerals*: OrderedTable[int, string]

   Parameter* = ref object
      pos*: Position
      name*: string
      dtype*: Dtype

   Value* = ref object
      pos*: Position
      dtype*: Dtype
      valBool*: bool
      valInt*: int
      valStr*: string
      valEnum*: int
      structMembers*: OrderedTable[string, Value]

   IdentifierKind* = enum
      dtypeIdentifier,
      variableIdentifier

   Identifier* = ref object
      name*: string
      pos*: Position
      kind*: IdentifierKind
      dtype*: Dtype

func `==`*(l, r: Dtype): bool =
   if l.base != r.base: return false
   # TODO: Check equality for derived types!
   return true


func `$`*(dtype: Dtype): string =
   case dtype.base
   of terror, tvoid, tbool, tint, tstr: return $dtype.base
   of tfunc: return "[func]"
   of tstruct: return "[struct]"
   of tenum: return "[enum]"

func `$`*(val: Value): string =
   case val.dtype.base
   of terror: return "[error]"
   of tvoid: return "[void]"
   of tbool: return $val.valBool
   of tint: return $val.valInt
   of tstr: return $val.valStr
   of tfunc: return "[func]"
   of tstruct: return "[struct]"
   of tenum: return "[enum]"

func `$`*(id: Identifier): string

func `$`*(parameter: Parameter): string =
   return parameter.name & ": " & $parameter.dtype

func `$`*(id: Identifier): string =
   case id.kind
   of variableIdentifier: return id.name & ": " & $id.dtype
   of dtypeIdentifier:
      if id.dtype.base in basicDtypes: return $id.dtype.base
      else: return id.name & ": " & $id.dtype.base

func newDtypeIdentifier*(name: string, pos: Position): Identifier =
   return Identifier(kind: dtypeIdentifier, pos: pos)

func newVariableIdentifier*(name: string, dtype: Dtype, pos: Position): Identifier =
   return Identifier(kind: variableIdentifier, name: name, dtype: dtype, pos: pos)
