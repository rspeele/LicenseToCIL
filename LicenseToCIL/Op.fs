module LicenseToCIL.Op
open LicenseToCIL
open LicenseToCIL.Stack
open System.Reflection.Emit

let inline pops stack f (il : IL) =
    f il.Generator
    stack |> popped

let inline pops2 stack f (il : IL) =
    f il.Generator
    stack |> popped |> popped

let inline pushes stack f (il : IL) =
    f il.Generator
    stack |> pushed

let inline binop opcode stack (il : IL) =
    il.Generator.Emit(opcode)
    stack |> popped |> popped |> pushed

let inline unop opcode stack (il : IL) =
    il.Generator.Emit(opcode)
    stack |> popped |> pushed

////////////////////////////////////////
// Primitive stack operations
////////////////////////////////////////

let pop stack = pops stack (fun g -> g.Emit(OpCodes.Pop))

let dup (stack : 'x S S) = pushes stack (fun g -> g.Emit(OpCodes.Dup))

////////////////////////////////////////
// Loading constants
////////////////////////////////////////

let ldc'i4 (i : int) stack =
    pushes stack <| fun il ->
    match i with
    | -1 -> il.Emit(OpCodes.Ldc_I4_M1)
    | 0 -> il.Emit(OpCodes.Ldc_I4_0)
    | 1 -> il.Emit(OpCodes.Ldc_I4_1)
    | 2 -> il.Emit(OpCodes.Ldc_I4_2)
    | 3 -> il.Emit(OpCodes.Ldc_I4_3)
    | 4 -> il.Emit(OpCodes.Ldc_I4_4)
    | 5 -> il.Emit(OpCodes.Ldc_I4_5)
    | 6 -> il.Emit(OpCodes.Ldc_I4_6)
    | 7 -> il.Emit(OpCodes.Ldc_I4_7)
    | 8 -> il.Emit(OpCodes.Ldc_I4_8)
    | s when s >= -128 && s < 128 -> il.Emit(OpCodes.Ldc_I4_S, sbyte s)
    | i -> il.Emit(OpCodes.Ldc_I4, i)

let ldc'i8 (i : int64) stack =
    pushes stack <| fun il -> il.Emit(OpCodes.Ldc_I8, i)

let ldc'r4 (r : single) stack =
    pushes stack <| fun il -> il.Emit(OpCodes.Ldc_R4, r)

let ldc'r8 (r : double) stack =
    pushes stack <| fun il -> il.Emit(OpCodes.Ldc_R8, r)

let ldstr (s: string) stack =
    pushes stack <| fun il -> il.Emit(OpCodes.Ldstr, s)

////////////////////////////////////////
// Arithmetic and logic
////////////////////////////////////////

let add stack = binop OpCodes.Add stack
let add'ovf stack = binop OpCodes.Add_Ovf stack
let add'ovf'un stack = binop OpCodes.Add_Ovf_Un stack

let sub stack = binop OpCodes.Sub stack
let sub'ovf stack = binop OpCodes.Sub_Ovf stack
let sub'ovf'un stack = binop OpCodes.Sub_Ovf_Un stack

let mul stack = binop OpCodes.Mul stack
let mul'ovf stack = binop OpCodes.Mul_Ovf stack
let mul'ovf'un stack = binop OpCodes.Mul_Ovf_Un stack

let div stack = binop OpCodes.Div stack
let div'un stack = binop OpCodes.Div_Un stack

let rem stack = binop OpCodes.Rem stack
let rem'un stack = binop OpCodes.Rem_Un stack

let neg stack = unop OpCodes.Neg stack

let shr stack = binop OpCodes.Shr stack
let shr'un stack = binop OpCodes.Shr_Un stack
let shl stack = binop OpCodes.Shl stack

let bitwise'and stack = binop OpCodes.And stack
let bitwise'or stack = binop OpCodes.Or stack
let bitwise'xor stack = binop OpCodes.Xor stack
let bitwise'not stack = unop OpCodes.Not stack

////////////////////////////////////////
// Locals
////////////////////////////////////////
    
let ldloc (local : LocalBuilder) stack =
    pushes stack <| fun il ->
    match local.LocalIndex with
    | 0 -> il.Emit(OpCodes.Ldloc_0)
    | 1 -> il.Emit(OpCodes.Ldloc_1)
    | 2 -> il.Emit(OpCodes.Ldloc_2)
    | 3 -> il.Emit(OpCodes.Ldloc_3)
    | s when s < 256 -> il.Emit(OpCodes.Ldloc_S, byte s)
    | i -> il.Emit(OpCodes.Ldloc, int16 i)

let ldloca (local : LocalBuilder) stack =
    pushes stack <| fun il ->
    match local.LocalIndex with
    | s when s < 256 -> il.Emit(OpCodes.Ldloca_S, byte s)
    | i -> il.Emit(OpCodes.Ldloca, int16 i)

let stloc (local : LocalBuilder) stack =
    pops stack <| fun il ->
    match local.LocalIndex with
    | 0 -> il.Emit(OpCodes.Stloc_0)
    | 1 -> il.Emit(OpCodes.Stloc_1)
    | 2 -> il.Emit(OpCodes.Stloc_2)
    | 3 -> il.Emit(OpCodes.Stloc_3)
    | s when s < 256 -> il.Emit(OpCodes.Stloc_S, byte s)
    | i -> il.Emit(OpCodes.Stloc, i)

////////////////////////////////////////
// Labels and branching
////////////////////////////////////////

type Label<'stack> = Label of Label

type LabelDefinition = internal | LabelDefinition

let deflabel = LabelDefinition

let mark (Label lbl : 'x S Label) (stack : 'x S) (il : IL) =
    il.Generator.MarkLabel(lbl)
    stack

let br (Label lbl : 'x S Label) (stack : 'x S) (il : IL) =
    il.Generator.Emit(OpCodes.Br, lbl)
    // don't enforce anything about the following stack
    // since we're jumping away unconditionally
    null : S<'b>

let br's (Label lbl : 'x S Label) (stack : 'x S) (il : IL) =
    il.Generator.Emit(OpCodes.Br_S, lbl)
    null : S<'b>

let brtrue (Label lbl : 'x S Label) (stack : 'x S S) =
    pops stack <| fun il -> il.Emit(OpCodes.Brtrue, lbl)

let brfalse (Label lbl : 'x S Label) (stack : 'x S S) =
    pops stack <| fun il -> il.Emit(OpCodes.Brfalse, lbl)

let brtrue's (Label lbl : 'x S Label) (stack : 'x S S) =
    pops stack <| fun il -> il.Emit(OpCodes.Brtrue_S, lbl)

let brfalse's (Label lbl : 'x S Label) (stack : 'x S S) =
    pops stack <| fun il -> il.Emit(OpCodes.Brfalse_S, lbl)

let beq (Label lbl : 'x S Label) (stack : 'x S S S) =
    pops2 stack <| fun il -> il.Emit(OpCodes.Beq, lbl)

let beq's (Label lbl : 'x S Label) (stack : 'x S S S) =
    pops2 stack <| fun il -> il.Emit(OpCodes.Beq_S, lbl)

