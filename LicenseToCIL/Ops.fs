module LicenseToCIL.Ops
open LicenseToCIL
open LicenseToCIL.Stack
open System
open System.Reflection
open System.Reflection.Emit
open System.ComponentModel

type Local = LocalBuilder

let inline combine (op1 : Op<'inp, 'mid>) (op2 : unit -> Op<'mid, 'out>) : Op<'inp, 'out> =
    fun _ _ il ->
        op1 null null il
        op2 () null null il

type CILHelpers = // making these normal static methods instead of let-bound functions prevents confusing inliner
    static member LdLoc (local : LocalBuilder, il : ILGenerator) =
        match local.LocalIndex with
        | 0 -> il.Emit(OpCodes.Ldloc_0)
        | 1 -> il.Emit(OpCodes.Ldloc_1)
        | 2 -> il.Emit(OpCodes.Ldloc_2)
        | 3 -> il.Emit(OpCodes.Ldloc_3)
        | s when s < 256 -> il.Emit(OpCodes.Ldloc_S, byte s)
        | i -> il.Emit(OpCodes.Ldloc, int16 i)
    static member LdLoca(local : LocalBuilder, il : ILGenerator) =
        match local.LocalIndex with
        | s when s < 256 -> il.Emit(OpCodes.Ldloca_S, byte s)
        | i -> il.Emit(OpCodes.Ldloca, int16 i)
    static member StLoc(local : LocalBuilder, il : ILGenerator) =
        match local.LocalIndex with
        | 0 -> il.Emit(OpCodes.Stloc_0)
        | 1 -> il.Emit(OpCodes.Stloc_1)
        | 2 -> il.Emit(OpCodes.Stloc_2)
        | 3 -> il.Emit(OpCodes.Stloc_3)
        | s when s < 256 -> il.Emit(OpCodes.Stloc_S, byte s)
        | i -> il.Emit(OpCodes.Stloc, i)
    static member LdcI4(i : int, il : ILGenerator) =
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

[<Obsolete>]
let inline private nops (f : ILGenerator -> unit) (_ : 'x S) (_ : 'x S) (il : IL) =
    f il.Generator

[<Obsolete>]
let inline private plus1in (op : Op<'x, 'y>) : Op<'x S, 'y> =
    fun (_ : 'x S S) (_ : 'y S) il ->
        op null null il

[<Obsolete>]
let inline private plus1out (op : Op<'x, 'y>) : Op<'x, 'y S> =
    fun (_ : 'x S) (_ : 'y S S) il ->
        op null null il

[<Obsolete>]
let inline private pops f = nops f |> plus1in
[<Obsolete>]
let inline private pops2 f = pops f |> plus1in
[<Obsolete>]
let inline private pops3 f = pops2 f |> plus1in
[<Obsolete>]
let inline private pops1pushes1 f = pops f |> plus1out
[<Obsolete>]
let inline private pops2pushes1 f = pops2 f |> plus1out
[<Obsolete>]
let inline private pushes f = nops f |> plus1out
[<Obsolete>]
let inline private binop opcode = pops2pushes1 (fun il -> il.Emit(opcode))
[<Obsolete>]
let inline private binop0 opcode = pops2 (fun il -> il.Emit(opcode))
[<Obsolete>]
let inline private unop opcode = pops1pushes1 (fun il -> il.Emit(opcode))

let inline zero (_ : 'x S) (_ : 'x S) (_ : IL) =
    ()

let inline pretend (_ : 'x S) (_ : 'y S) (_ : IL) = ()

////////////////////////////////////////
// Calls
////////////////////////////////////////

let call'x (meth : MethodInfo) (_ : 'x S) (_ : 'y S) (il : IL) =
    il.Generator.Emit(OpCodes.Call, meth)

let call0 meth : Op<'x, 'x S> = call'x meth
let call1 meth : Op<'x S, 'x S> = call'x meth
let call2 meth : Op<'x S S, 'x S> = call'x meth
let call3 meth : Op<'x S S S, 'x S> = call'x meth
let call4 meth : Op<'x S S S S, 'x S> = call'x meth
let call5 meth : Op<'x S S S S S, 'x S> = call'x meth
let call6 meth : Op<'x S S S S S S, 'x S> = call'x meth

let call0'void meth : Op<'x, 'x> = call'x meth
let call1'void meth : Op<'x S, 'x> = call'x meth
let call2'void meth : Op<'x S S, 'x> = call'x meth
let call3'void meth : Op<'x S S S, 'x> = call'x meth
let call4'void meth : Op<'x S S S S, 'x> = call'x meth
let call5'void meth : Op<'x S S S S S, 'x> = call'x meth
let call6'void meth : Op<'x S S S S S S, 'x> = call'x meth

let calli'x (_ : 'x S S) (_ : 'y S) (il : IL) =
    il.Generator.Emit(OpCodes.Calli)

let calli0 : Op<'x S, 'x S> = fun st -> calli'x st
let calli1 : Op<'x S S, 'x S> = fun st -> calli'x st
let calli2 : Op<'x S S S, 'x S> = fun st -> calli'x st
let calli3 : Op<'x S S S S, 'x S> = fun st -> calli'x st
let calli4 : Op<'x S S S S S, 'x S> = fun st -> calli'x st
let calli5 : Op<'x S S S S S S, 'x S> = fun st -> calli'x st

let calli0'void : Op<'x S, 'x S> = fun st -> calli'x st
let calli1'void : Op<'x S S, 'x S> = fun st -> calli'x st
let calli2'void : Op<'x S S S, 'x S> = fun st -> calli'x st
let calli3'void : Op<'x S S S S, 'x S> = fun st -> calli'x st
let calli4'void : Op<'x S S S S S, 'x S> = fun st -> calli'x st
let calli5'void : Op<'x S S S S S S, 'x S> = fun st -> calli'x st

let callvirt'x (meth : MethodInfo) (_ : 'x S) (_ : 'y S) (il : IL) =
    il.Generator.Emit(OpCodes.Callvirt, meth)

let callvirt1 meth : Op<'x S, 'x S> = callvirt'x meth
let callvirt2 meth : Op<'x S S, 'x S> = callvirt'x meth
let callvirt3 meth : Op<'x S S S, 'x S> = callvirt'x meth
let callvirt4 meth : Op<'x S S S S, 'x S> = callvirt'x meth
let callvirt5 meth : Op<'x S S S S S, 'x S> = callvirt'x meth

let callvirt1'void meth : Op<'x S, 'x> = callvirt'x meth
let callvirt2'void meth : Op<'x S S, 'x> = callvirt'x meth
let callvirt3'void meth : Op<'x S S S, 'x> = callvirt'x meth
let callvirt4'void meth : Op<'x S S S S, 'x> = callvirt'x meth
let callvirt5'void meth : Op<'x S S S S S, 'x> = callvirt'x meth

let newobj'x (cons : ConstructorInfo) (_ : 'x S) (_ : 'y S S) (il : IL) =
    il.Generator.Emit(OpCodes.Newobj, cons)

let newobj0 cons : Op<'x, 'x S> = newobj'x cons
let newobj1 cons : Op<'x S, 'x S> = newobj'x cons
let newobj2 cons : Op<'x S S, 'x S> = newobj'x cons
let newobj3 cons : Op<'x S S S, 'x S> = newobj'x cons
let newobj4 cons : Op<'x S S S S, 'x S> = newobj'x cons
let newobj5 cons : Op<'x S S S S S, 'x S> = newobj'x cons

////////////////////////////////////////
// Primitive stack operations
////////////////////////////////////////

let nop stackin = (nops <| fun il -> il.Emit(OpCodes.Nop)) stackin

let pop stackin = (pops (fun g -> g.Emit(OpCodes.Pop))) stackin

let dup (_ : 'x S S) (_ : 'x S S S) (il : IL) = il.Generator.Emit(OpCodes.Dup)

let ret (_ : E S S) (_ : 'x S) (il : IL) =
    il.Generator.Emit(OpCodes.Ret)

let ret'void (_ : E S) (_ : 'x S) (il : IL) =
    il.Generator.Emit(OpCodes.Ret)

////////////////////////////////////////
// Loading constants
////////////////////////////////////////

let ldnull (_ : 'x S) (_ : 'x S S) (il : IL) =
    il.Generator.Emit(OpCodes.Ldnull)

let inline ldc'i4 (i : int) (_ : 'x S) (_ : 'x S S) (il : IL) =
    CILHelpers.LdcI4(i, il.Generator)

let inline ldc'i8 (i : int64) (_ : 'x S) (_ : 'x S S) (il : IL) =
    il.Generator.Emit(OpCodes.Ldc_I8, i)

let inline ldc'r4 (r : single) (_ : 'x S) (_ : 'x S S) (il : IL) =
    il.Generator.Emit(OpCodes.Ldc_R4, r)

let inline ldc'r8 (r : double) (_ : 'x S) (_ : 'x S S) (il : IL) =
    il.Generator.Emit(OpCodes.Ldc_R8, r)

let inline ldstr (s : string) (_ : 'x S) (_ : 'x S S) (il : IL) =
    il.Generator.Emit(OpCodes.Ldstr, s)

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

let bit'and stack = binop OpCodes.And stack
let bit'or stack = binop OpCodes.Or stack
let bit'xor stack = binop OpCodes.Xor stack
let bit'not stack = unop OpCodes.Not stack

////////////////////////////////////////
// Comparison
////////////////////////////////////////

let ceq stack = binop OpCodes.Ceq stack
let cgt stack = binop OpCodes.Cgt stack
let cgt'un stack = binop OpCodes.Cgt_Un stack
let clt stack = binop OpCodes.Clt stack
let clt'un stack = binop OpCodes.Clt_Un stack

let ckfinite stack = (pops <| fun il -> il.Emit(OpCodes.Ckfinite)) stack

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
let conv'r'un stack = unop OpCodes.Conv_R_Un stack
let conv'r8 stack = unop OpCodes.Conv_R8 stack

////////////////////////////////////////
// Arrays
////////////////////////////////////////

let newarr (elemTy : Type) = pops1pushes1 (fun il -> il.Emit(OpCodes.Newarr, elemTy))

let ldlen stack = unop OpCodes.Ldlen stack

/// [array,index] -> [value]
let ldelem (elemTy : Type)  = pops2pushes1 <| fun il -> il.Emit(OpCodes.Ldelem, elemTy)
let ldelem'i stack = binop OpCodes.Ldelem_I stack
let ldelem'i1 stack = binop OpCodes.Ldelem_I1 stack
let ldelem'i2 stack = binop OpCodes.Ldelem_I2 stack
let ldelem'i4 stack = binop OpCodes.Ldelem_I4 stack
let ldelem'i8 stack = binop OpCodes.Ldelem_I8 stack
let ldelem'u1 stack = binop OpCodes.Ldelem_U1 stack
let ldelem'u2 stack = binop OpCodes.Ldelem_U2 stack
let ldelem'u4 stack = binop OpCodes.Ldelem_U4 stack
let ldelem'u8 stack = binop OpCodes.Ldelem_I8 stack
let ldelem'r4 stack = binop OpCodes.Ldelem_R4 stack
let ldelem'r8 stack = binop OpCodes.Ldelem_R8 stack
let ldelem'ref stack = binop OpCodes.Ldelem_Ref stack
let ldelema (elemTy : Type) = pops2pushes1 <| fun il -> il.Emit(OpCodes.Ldelema, elemTy)

/// [array,index,value] -> []
let stelem (ty : Type) = pops3 <| fun il -> il.Emit(OpCodes.Stelem, ty)
let stelem'i stack = (pops3 <| fun il -> il.Emit(OpCodes.Stelem_I)) stack
let stelem'i1 stack = (pops3 <| fun il -> il.Emit(OpCodes.Stelem_I1)) stack
let stelem'i2 stack = (pops3 <| fun il -> il.Emit(OpCodes.Stelem_I2)) stack
let stelem'i4 stack = (pops3 <| fun il -> il.Emit(OpCodes.Stelem_I4)) stack
let stelem'i8 stack = (pops3 <| fun il -> il.Emit(OpCodes.Stelem_I8)) stack
let stelem'r4 stack = (pops3 <| fun il -> il.Emit(OpCodes.Stelem_R4)) stack
let stelem'r8 stack = (pops3 <| fun il -> il.Emit(OpCodes.Stelem_R8)) stack
let stelem'ref stack = (pops3 <| fun il -> il.Emit(OpCodes.Stelem_Ref)) stack

////////////////////////////////////////
// Arguments
////////////////////////////////////////

let ldarg i =
    pushes <| fun il ->
    match i with
    | 0 -> il.Emit(OpCodes.Ldarg_0)
    | 1 -> il.Emit(OpCodes.Ldarg_1)
    | 2 -> il.Emit(OpCodes.Ldarg_2)
    | 3 -> il.Emit(OpCodes.Ldarg_3)
    | s when s < 256 -> il.Emit(OpCodes.Ldarg_S, byte s)
    | i -> il.Emit(OpCodes.Ldarg, i)

let ldarga i =
    pushes <| fun il ->
    match i with
    | s when s < 256 -> il.Emit(OpCodes.Ldarga_S, byte s)
    | i -> il.Emit(OpCodes.Ldarga, i)

let starg i =
    pops <| fun il ->
    match i with
    | s when s < 256 -> il.Emit(OpCodes.Starg_S, byte s)
    | i -> il.Emit(OpCodes.Starg, i)

////////////////////////////////////////
// Fields
////////////////////////////////////////

let stfld (field : FieldInfo) = pops2 <| fun il -> il.Emit(OpCodes.Stfld, field)
let stsfld (field : FieldInfo) = pops <| fun il -> il.Emit(OpCodes.Stsfld, field)
let ldfld (field : FieldInfo) = pops1pushes1 <| fun il -> il.Emit(OpCodes.Ldfld, field)
let ldsfld (field : FieldInfo) = pushes <| fun il -> il.Emit(OpCodes.Ldsfld, field)
let ldflda (field : FieldInfo) = pops1pushes1 <| fun il -> il.Emit(OpCodes.Ldflda, field)
let ldsflda (field : FieldInfo) = pushes <| fun il -> il.Emit(OpCodes.Ldsflda, field)

////////////////////////////////////////
// Indirection
////////////////////////////////////////

let stind'i stack = binop0 OpCodes.Stind_I stack
let stind'i1 stack = binop0 OpCodes.Stind_I1 stack
let stind'i2 stack = binop0 OpCodes.Stind_I2 stack
let stind'i4 stack = binop0 OpCodes.Stind_I4 stack
let stind'i8 stack = binop0 OpCodes.Stind_I8 stack
let stind'r4 stack = binop0 OpCodes.Stind_R4 stack
let stind'r8 stack = binop0 OpCodes.Stind_R8 stack
let stind'ref stack = binop0 OpCodes.Stind_Ref stack

let ldind'i stack = unop OpCodes.Ldind_I stack
let ldind'i1 stack = unop OpCodes.Ldind_I1 stack
let ldind'i2 stack = unop OpCodes.Ldind_I2 stack
let ldind'i4 stack = unop OpCodes.Ldind_I4 stack
let ldind'i8 stack = unop OpCodes.Ldind_I8 stack
let ldind'r4 stack = unop OpCodes.Ldind_R4 stack
let ldind'r8 stack = unop OpCodes.Ldind_R8 stack
let ldind'ref stack = unop OpCodes.Ldind_Ref stack
let ldind'u1 stack = unop OpCodes.Ldind_U1 stack
let ldind'u2 stack = unop OpCodes.Ldind_U2 stack
let ldind'u4 stack = unop OpCodes.Ldind_U4 stack

////////////////////////////////////////
// Locals
////////////////////////////////////////

type LocalDefinition = internal | LocalDefinition of Type
type LocalTemporary = internal | LocalTemporary of Type

let deflocal ty = LocalDefinition ty
let tmplocal ty = LocalTemporary ty
    
let inline ldloc (local : LocalBuilder) (_ : 'x S) (_ : 'x S S) (il : IL) =
    CILHelpers.LdLoc(local, il.Generator)

let inline ldloca (local : LocalBuilder) (_ : 'x S) (_ : 'x S S) (il : IL) =
    CILHelpers.LdLoca(local, il.Generator)
    
let inline stloc (local : LocalBuilder) (_ : 'x S S) (_ : 'x S) (il : IL) =
    CILHelpers.StLoc(local, il.Generator)

////////////////////////////////////////
// Labels and branching
////////////////////////////////////////

type Label<'stack> = Label of Label

type LabelDefinition = internal | LabelDefinition

let deflabel = LabelDefinition

let mark (Label lbl : 'x Label) (_ : 'x S) (_ : 'x S) (il : IL) =
    il.Generator.MarkLabel(lbl)

// don't enforce anything about the following stack
// since we're jumping away unconditionally
let br (Label lbl : 'x Label) (_ : 'x S) (_ : _ S) (il : IL) =
    il.Generator.Emit(OpCodes.Br, lbl)

let br's (Label lbl : 'x Label) (_ : 'x S) (_ : _ S) (il : IL) =
    il.Generator.Emit(OpCodes.Br_S, lbl)

let leave (Label lbl : E Label) (_ : 'x S) (_ : _ S) (il : IL) =
    il.Generator.Emit(OpCodes.Leave, lbl)

let leave's (Label lbl : E Label) (_ : 'x S) (_ : _ S) (il : IL) =
    il.Generator.Emit(OpCodes.Leave, lbl)

/// Conditional branch that pops one element off the stack.
let inline private cbr1 opcode (Label lbl : 'x Label) (_ : 'x S S) (_ : 'x S) (il : IL) =
    il.Generator.Emit(opcode, lbl)

/// Conditional branch that pops two elements off the stack.
let inline private cbr2 opcode (Label lbl : 'x Label) (_ : 'x S S S) (_ : 'x S) (il : IL) =
    il.Generator.Emit(opcode, lbl)

let brtrue label = cbr1 OpCodes.Brtrue label
let brtrue's label = cbr1 OpCodes.Brtrue_S label

let brfalse label = cbr1 OpCodes.Brfalse label
let brfalse's label = cbr1 OpCodes.Brfalse_S label

let beq label = cbr2 OpCodes.Beq label
let beq's label = cbr2 OpCodes.Beq_S label

let bne'un label = cbr2 OpCodes.Bne_Un label
let bne'un's label = cbr2 OpCodes.Bne_Un_S label

let bge label = cbr2 OpCodes.Bge label
let bge's label = cbr2 OpCodes.Bge_S label
let bge'un label = cbr2 OpCodes.Bge_Un label
let bge'un's label = cbr2 OpCodes.Bge_Un_S label

let ble label = cbr2 OpCodes.Ble label
let ble's label = cbr2 OpCodes.Ble_S label
let ble'un label = cbr2 OpCodes.Ble_Un label
let ble'un's label = cbr2 OpCodes.Ble_Un_S label

let bgt label = cbr2 OpCodes.Bgt label
let bgt's label = cbr2 OpCodes.Bgt_S label
let bgt'un label = cbr2 OpCodes.Bgt_Un label
let bgt'un's label = cbr2 OpCodes.Bgt_Un_S label

let blt label = cbr2 OpCodes.Blt label
let blt's label = cbr2 OpCodes.Blt_S label
let blt'un label = cbr2 OpCodes.Blt_Un label
let blt'un's label = cbr2 OpCodes.Blt_Un_S label

let switch (labels : 'x Label seq) (_ : 'y S S) (_ : 'x S) (il : IL) =
    let labels = [| for Label lbl in labels -> lbl |]
    il.Generator.Emit(OpCodes.Switch, labels)

////////////////////////////////////////
// Type system
////////////////////////////////////////

let ldtoken (ty : Type) = pushes <| fun il -> il.Emit(OpCodes.Ldtoken, ty)
let ldftn (meth : MethodInfo) = pushes <| fun il -> il.Emit(OpCodes.Ldftn, meth)
let ldvirtftn (meth : MethodInfo) = pops1pushes1 <| fun il -> il.Emit(OpCodes.Ldvirtftn, meth)
let box'val (ty : Type) = pops1pushes1 <| fun il -> il.Emit(OpCodes.Box, ty)
let unbox'any (ty : Type) = pops1pushes1 <| fun il -> il.Emit(OpCodes.Unbox_Any, ty)
let unbox'val (ty : Type) = pops1pushes1 <| fun il -> il.Emit(OpCodes.Unbox, ty)
let castclass (ty : Type) = pops1pushes1 <| fun il -> il.Emit(OpCodes.Castclass, ty)
let isinst (ty : Type) = pops1pushes1 <| fun il -> il.Emit(OpCodes.Isinst, ty)
let refanyval (ty : Type) = pops1pushes1 <| fun il -> il.Emit(OpCodes.Refanyval, ty)
let refanytype stack = (pops1pushes1 <| fun il -> il.Emit(OpCodes.Refanytype)) stack
let initobj (ty : Type) = pops <| fun il -> il.Emit(OpCodes.Initobj, ty)
let mkrefany (ty : Type) = pops1pushes1 <| fun il -> il.Emit(OpCodes.Mkrefany, ty)

////////////////////////////////////////
// Exception stuff
////////////////////////////////////////

let endfilter stack = (pops <| fun il -> il.Emit(OpCodes.Endfilter)) stack
let endfinally stack = (nops <| fun il -> il.Emit(OpCodes.Endfinally)) stack
let rethrow stack = (nops <| fun il -> il.Emit(OpCodes.Rethrow)) stack
let throw (_ : 'x S S) (_ : _ S) (il : IL) =
    il.Generator.Emit(OpCodes.Throw)

////////////////////////////////////////
// Weird stuff and memory operations
////////////////////////////////////////

let arglist stack = (pushes <| fun il -> il.Emit(OpCodes.Arglist)) stack
let cpblk stack = (pops3 <| fun il -> il.Emit(OpCodes.Cpblk)) stack
let stobj (ty : Type) = pops2 <| fun il -> il.Emit(OpCodes.Stobj, ty)
let ldobj (ty : Type) = pops1pushes1 <| fun il -> il.Emit(OpCodes.Ldobj, ty)
let cpobj (ty : Type) = pops2 <| fun il -> il.Emit(OpCodes.Cpobj, ty)
let sizeof (ty : Type) = pushes <| fun il -> il.Emit(OpCodes.Sizeof, ty)
let jmp (meth : MethodInfo) (_ : E S) (_ : E S) (il : IL) =
    il.Generator.Emit(OpCodes.Jmp, meth)
let localloc stack = (unop OpCodes.Localloc) stack
let initblk stack = (pops3 <| fun il -> il.Emit(OpCodes.Initblk)) stack

////////////////////////////////////////
// Prefixes
////////////////////////////////////////

let constrained (ty : Type) = nops <| fun il -> il.Emit(OpCodes.Constrained, ty)
let tail stack = (nops <| fun il -> il.Emit(OpCodes.Tailcall)) stack
let unaligned stack = (pops1pushes1 <| fun il -> il.Emit(OpCodes.Unaligned)) stack
let volatile' stack = (pops1pushes1 <| fun il -> il.Emit(OpCodes.Volatile)) stack