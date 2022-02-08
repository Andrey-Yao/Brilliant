
type var = string
type label = string

type binop = Add | Mul | Sub | Div | Eq | Lt | Gt
             | Le | Ge | And | Or | PtrAdd
type const = CInt of int | CBool of bool | CFloat of float
type unaop = Load | Not | Id | Alloc

type bril_type = TInt | TBool | TFloat | TPtr of bril_type

(* No side effects when executed! *)
type bril_expr =
  | BinOp of binop * var * var
  | UnaOp of unaop * var
  | Const of const

type bril_inst =
  | ValInst of {dest: var; tipe: bril_type; expr: bril_expr}
  | CallEff of {dest: var; func: string; args: var list}
  | CallVal of {func: string; args: var list}
  | Alloc of {dest: var; tipe: bril_type; size: int}
  | Br of {cond: var; yes: label; nah: label}
  | Store of {pointer: var; input: var}
  | Print of var list
  | Ret of var option
  | Label of label
  | Jmp of label
  | Free of var
  | Nop


type bril_func =
  { name: string;
    args: (var * bril_type) list;
    instrs: bril_inst list;
    ret_type: bril_type option }
