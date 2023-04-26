import tables, hashes, strutils
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
      ttype = "type"
      tfunc = "func"
      tstruct = "struct"
      tenum = "enum"

const
   basicDtypes* = {tvoid, tbool, tint, tstr}

type
   Dtype* = ref object
      pos*: Position
      case base*: DtypeBase
      of terror, basicDtypes: discard
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

   Identifier* = ref object
      name*: string
      pos*: Position
      dtype*: Dtype

func hash*(self: Identifier): Hash =
   return cast[pointer](self).hash

func `==`*(l, r: Dtype): bool =
   if l.base != r.base: return false
   # TODO: Check equality for derived types!
   return true


func `$`*(dtype: Dtype): string =
   result = $dtype.base
   case dtype.base
   of terror, basicDtypes: discard
   of ttype:
      if not dtype.dtype.isNil: result &= "<" & $dtype.dtype & ">"
   of tfunc:
      if not dtype.retDtype.isNil:
         result &= "("
         for idx, p in dtype.parameters:
            if idx > 0: result &= ", "
            result &= p.name & ": " & $p.dtype
         result &= "): " & $dtype.retDtype
         # if dtype.hasImplementation: result &= " = [implementation]"
   of tstruct: discard
   of tenum: discard

func asTree*(dtype: Dtype): string = $dtype
func asCode*(dtype: Dtype): string = $dtype

func newDtype*(base: DtypeBase, pos: Position = Position()): Dtype =
   return Dtype(pos: pos, base: base)

func newDtype*(dtype: Dtype, pos: Position = Position()): Dtype =
   let myPos = if pos.abs != 0: pos else: dtype.pos
   result = Dtype(base: dtype.base, pos: myPos)
   case result.base
   of terror, basicDtypes: discard
   of ttype:
      result.dtype = dtype.dtype
   of tfunc:
      result.retDtype = dtype.retDtype
      result.parameters = dtype.parameters
      result.hasImplementation = dtype.hasImplementation # tbd
   of tstruct:
      result.members = dtype.members
   of tenum:
      result.enumerals = dtype.enumerals


func `$`*(id: Identifier): string =
   return id.name & ": " & $id.dtype

func asTree*(id: Identifier): string = $id
func asCode*(id: Identifier): string = id.name


type
   Value* = ref object
      pos*: Position
      dtype*: Dtype
      valBool*: bool
      valInt*: int
      valStr*: string
      valEnum*: int
      structMembers*: seq[Value]


func `$`*(val: Value): string =
   case val.dtype.base
   of terror: return "[error]"
   of tvoid: return "[void]"
   of tbool: return $val.valBool
   of tint: return $val.valInt
   of tstr: return $val.valStr
   of ttype: return $val.dtype
   of tfunc: return "[func]"
   of tstruct:
      var res: seq[string]
      for member in val.structMembers:
         res.add($member)
      return "{" & res.join(", ") & "}"
   of tenum: return "[enum]"

func asCode*(val: Value): string = $val
func asTree*(val: Value): string = $val
