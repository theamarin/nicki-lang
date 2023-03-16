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
      of tstruct:
         members*: OrderedTable[string, Dtype]
      of tenum:
         enumerals*: OrderedTable[int, string]

   Identifier* = ref object
      name*: string
      pos*: Position
      dtype*: Dtype

   Value* = ref object
      pos*: Position
      dtype*: Dtype
      valBool*: bool
      valInt*: int
      valStr*: string
      valDtype*: Dtype
      valEnum*: int
      structMembers*: OrderedTable[string, Value]



func `==`*(l, r: Dtype): bool =
   if l.base != r.base: return false
   # TODO: Check equality for derived types!
   return true


func `$`*(dtype: Dtype): string =
   result = $dtype.base
   case dtype.base
   of terror, tvoid, tbool, tint, tstr: discard
   of ttype:
      if not dtype.dtype.isNil: result &= "<" & $dtype.dtype & ">"
   of tfunc:
      if not dtype.retDtype.isNil:
         result &= "("
         for idx, p in dtype.parameters:
            if idx > 0: result &= ", "
            result &= p.name & ": " & $p.dtype
         result &= "):" & $dtype.retDtype
   of tstruct: discard
   of tenum: discard

func `$`*(val: Value): string =
   case val.dtype.base
   of terror: return "[error]"
   of tvoid: return "[void]"
   of tbool: return $val.valBool
   of tint: return $val.valInt
   of tstr: return $val.valStr
   of ttype: return $val.valDtype
   of tfunc: return "[func]"
   of tstruct: return "[struct]"
   of tenum: return "[enum]"

func `$`*(id: Identifier): string =
   return id.name & ": " & $id.dtype
