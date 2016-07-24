module LicenseToCIL.Op
open LicenseToCIL
open LicenseToCIL.Stack
open System
open System.Reflection.Emit

let inline private pops stack f (il : IL) =
    f il.Generator
    stack |> popped

let inline private pops2 stack f (il : IL) = pops stack f il |> popped
let inline private pops3 stack f (il : IL) = pops2 stack f il |> popped
let inline pops2pushes1 stack f =
    pops2 stack f >> pushed

let inline private pushes stack f (il : IL) =
    f il.Generator
    stack |> pushed

let inline private binop opcode stack (il : IL) =
    il.Generator.Emit(opcode)
    stack |> popped |> popped |> pushed

let inline private unop opcode stack (il : IL) =
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

let ldnull stack =
    pushes stack <| fun il -> il.Emit(OpCodes.Ldnull)

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
// Comparison
////////////////////////////////////////

let ceq stack = binop OpCodes.Ceq stack
let cgt stack = binop OpCodes.Cgt stack
let cgt'un stack = binop OpCodes.Cgt_Un stack
let clt stack = binop OpCodes.Clt stack
let clt'un stack = binop OpCodes.Clt_Un stack

let ckfinite stack = unop OpCodes.Ckfinite stack

////////////////////////////////////////
// Conversion
////////////////////////////////////////

let conv'i stack = unop OpCodes.Conv_I stack
let conv'i1 stack = unop OpCodes.Conv_I1 stack
let conv'i2 stack = unop OpCodes.Conv_I2 stack
let conv'i4 stack = unop OpCodes.Conv_I4 stack
let conv'i8 stack = unop OpCodes.Conv_I8 stack

let conv'ovf'i stack = unop OpCodes.Conv_Ovf_I stack
let conv'ovf'i1 stack = unop OpCodes.Conv_Ovf_I1 stack
let conv'ovf'i2 stack = unop OpCodes.Conv_Ovf_I2 stack
let conv'ovf'i4 stack = unop OpCodes.Conv_Ovf_I4 stack
let conv'ovf'i8 stack = unop OpCodes.Conv_Ovf_I8 stack

let conv'ovf'i'un stack = unop OpCodes.Conv_Ovf_I_Un stack
let conv'ovf'i1'un stack = unop OpCodes.Conv_Ovf_I1_Un stack
let conv'ovf'i2'un stack = unop OpCodes.Conv_Ovf_I2_Un stack
let conv'ovf'i4'un stack = unop OpCodes.Conv_Ovf_I4_Un stack
let conv'ovf'i8'un stack = unop OpCodes.Conv_Ovf_I8_Un stack

let conv'u stack = unop OpCodes.Conv_U stack
let conv'u1 stack = unop OpCodes.Conv_U1 stack
let conv'u2 stack = unop OpCodes.Conv_U2 stack
let conv'u4 stack = unop OpCodes.Conv_U4 stack
let conv'u8 stack = unop OpCodes.Conv_U8 stack

let conv'ovf'u stack = unop OpCodes.Conv_Ovf_U stack
let conv'ovf'u1 stack = unop OpCodes.Conv_Ovf_U1 stack
let conv'ovf'u2 stack = unop OpCodes.Conv_Ovf_U2 stack
let conv'ovf'u4 stack = unop OpCodes.Conv_Ovf_U4 stack
let conv'ovf'u8 stack = unop OpCodes.Conv_Ovf_U8 stack

let conv'ovf'u'un stack = unop OpCodes.Conv_Ovf_U_Un stack
let conv'ovf'u1'un stack = unop OpCodes.Conv_Ovf_U1_Un stack
let conv'ovf'u2'un stack = unop OpCodes.Conv_Ovf_U2_Un stack
let conv'ovf'u4'un stack = unop OpCodes.Conv_Ovf_U4_Un stack
let conv'ovf'u8'un stack = unop OpCodes.Conv_Ovf_U8_Un stack

let conv'r4 stack = unop OpCodes.Conv_R4 stack
let conv'r4'un stack = unop OpCodes.Conv_R_Un stack
let conv'r8 stack = unop OpCodes.Conv_R8 stack

////////////////////////////////////////
// Arrays
////////////////////////////////////////

/// [array,index,value] -> []
let stelem (ty : Type) stack = pops3 stack <| fun il -> il.Emit(OpCodes.Stelem, ty)
let stelem'i stack = pops3 stack <| fun il -> il.Emit(OpCodes.Stelem_I)
let stelem'i1 stack = pops3 stack <| fun il -> il.Emit(OpCodes.Stelem_I1)
let stelem'i2 stack = pops3 stack <| fun il -> il.Emit(OpCodes.Stelem_I2)
let stelem'i4 stack = pops3 stack <| fun il -> il.Emit(OpCodes.Stelem_I4)
let stelem'i8 stack = pops3 stack <| fun il -> il.Emit(OpCodes.Stelem_I8)
let stelem'r4 stack = pops3 stack <| fun il -> il.Emit(OpCodes.Stelem_R4)
let stelem'r8 stack = pops3 stack <| fun il -> il.Emit(OpCodes.Stelem_R8)
let stelem'ref stack = pops3 stack <| fun il -> il.Emit(OpCodes.Stelem_Ref)

/// [array,index] -> [value]
let ldelem (ty : Type) stack = pops2pushes1 stack <| fun il -> il.Emit(OpCodes.Ldelem, ty)
let ldelem'i stack = pops2pushes1 stack <| fun il -> il.Emit(OpCodes.Ldelem_I)
let ldelem'i1 stack = pops2pushes1 stack <| fun il -> il.Emit(OpCodes.Ldelem_I1)
let ldelem'i2 stack = pops2pushes1 stack <| fun il -> il.Emit(OpCodes.Ldelem_I2)
let ldelem'i4 stack = pops2pushes1 stack <| fun il -> il.Emit(OpCodes.Ldelem_I4)
let ldelem'i8 stack = pops2pushes1 stack <| fun il -> il.Emit(OpCodes.Ldelem_I8)
let ldelem'u1 stack = pops2pushes1 stack <| fun il -> il.Emit(OpCodes.Ldelem_U1)
let ldelem'u2 stack = pops2pushes1 stack <| fun il -> il.Emit(OpCodes.Ldelem_U2)
let ldelem'u4 stack = pops2pushes1 stack <| fun il -> il.Emit(OpCodes.Ldelem_U4)
let ldelem'u8 stack = pops2pushes1 stack <| fun il -> il.Emit(OpCodes.Ldelem_I8)
let ldelem'r4 stack = pops2pushes1 stack <| fun il -> il.Emit(OpCodes.Ldelem_R4)
let ldelem'r8 stack = pops2pushes1 stack <| fun il -> il.Emit(OpCodes.Ldelem_R8)
let ldelem'ref stack = pops2pushes1 stack <| fun il -> il.Emit(OpCodes.Ldelem_Ref)
let ldelema (ty : Type) stack = pops2pushes1 stack <| fun il -> il.Emit(OpCodes.Ldelema, ty)

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

/// Conditional branch that pops one element off the stack.
let inline private cbr1 opcode (Label lbl : 'x S Label) (stack : 'x S S) =
    pops stack <| fun il -> il.Emit(opcode, lbl)

/// Conditional branch that pops two elements off the stack.
let inline private cbr2 opcode (Label lbl : 'x S Label) (stack : 'x S S S) =
    pops2 stack <| fun il -> il.Emit(opcode, lbl)

let brtrue label = cbr1 OpCodes.Brtrue label
let brtrue's label = cbr1 OpCodes.Brtrue_S label

let brinst label = brtrue label
let brinst's label = brtrue's label

let brfalse label = cbr1 OpCodes.Brfalse label
let brfalse's label = cbr1 OpCodes.Brfalse_S label

let brnull label = brfalse label
let brnull's label = brfalse's label
let brzero label = brfalse label
let brzero's label = brfalse's label

let beq label = cbr2 OpCodes.Beq label
let beq's label = cbr2 OpCodes.Beq_S label

let bne'un label = cbr2 OpCodes.Bne_Un label
let bne'un's label = cbr2 OpCodes.Bne_Un_S label

let bge label = cbr2 OpCodes.Bge label
let bge's label = cbr2 OpCodes.Bge_S label
let bge'un label = cbr2 OpCodes.Bge_Un label
let bge'un's label = cbr2 OpCodes.Bge_Un_S

let ble label = cbr2 OpCodes.Ble label
let ble's label = cbr2 OpCodes.Ble_S label
let ble'un label = cbr2 OpCodes.Ble_Un label
let ble'un's label = cbr2 OpCodes.Ble_Un_S label

let bgt label = cbr2 OpCodes.Bgt label
let bgt's label = cbr2 OpCodes.Bgt_S label
let bgt'un label = cbr2 OpCodes.Bgt_Un label
let bgt'un's label = cbr2 OpCodes.Bgt_Un_S

let blt label = cbr2 OpCodes.Blt label
let blt's label = cbr2 OpCodes.Blt_S label
let blt'un label = cbr2 OpCodes.Blt_Un label
let blt'un's label = cbr2 OpCodes.Blt_Un_S





