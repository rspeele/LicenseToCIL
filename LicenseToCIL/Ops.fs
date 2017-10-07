module LicenseToCIL.Ops
open LicenseToCIL
open LicenseToCIL.Stack
open System
open System.Reflection
open System.Reflection.Emit

type Local = LocalBuilder

let inline combine (op1 : Op<'inp, 'mid>) (op2 : unit -> Op<'mid, 'out>) (_ : 'inp S) (_ : 'out S) il =
    op1 null null il
    op2 () null null il

type CILHelpers = // making these normal static methods instead of let-bound functions prevents confusing inliner
    static member LdLoc (local : LocalBuilder, il : IL) =
        match local.LocalIndex with
        | 0 -> il.Emit(OpCodes.Ldloc_0)
        | 1 -> il.Emit(OpCodes.Ldloc_1)
        | 2 -> il.Emit(OpCodes.Ldloc_2)
        | 3 -> il.Emit(OpCodes.Ldloc_3)
        | s when s < 256 -> il.EmitByte(OpCodes.Ldloc_S, byte s)
        | i -> il.EmitInt16(OpCodes.Ldloc, int16 i)
    static member LdLoca(local : LocalBuilder, il : IL) =
        match local.LocalIndex with
        | s when s < 256 -> il.EmitByte(OpCodes.Ldloca_S, byte s)
        | i -> il.EmitInt16(OpCodes.Ldloca, int16 i)
    static member StLoc(local : LocalBuilder, il : IL) =
        match local.LocalIndex with
        | 0 -> il.Emit(OpCodes.Stloc_0)
        | 1 -> il.Emit(OpCodes.Stloc_1)
        | 2 -> il.Emit(OpCodes.Stloc_2)
        | 3 -> il.Emit(OpCodes.Stloc_3)
        | s when s < 256 -> il.EmitByte(OpCodes.Stloc_S, byte s)
        | i -> il.EmitInt32(OpCodes.Stloc, i)
    static member LdcI4(i : int, il : IL) =
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
        | s when s >= -128 && s < 128 -> il.EmitSByte(OpCodes.Ldc_I4_S, sbyte s)
        | i -> il.EmitInt32(OpCodes.Ldc_I4, i)
    static member LdArg(i : int, il : IL) =
        match i with
        | 0 -> il.Emit(OpCodes.Ldarg_0)
        | 1 -> il.Emit(OpCodes.Ldarg_1)
        | 2 -> il.Emit(OpCodes.Ldarg_2)
        | 3 -> il.Emit(OpCodes.Ldarg_3)
        | s when s < 256 -> il.EmitByte(OpCodes.Ldarg_S, byte s)
        | i -> il.EmitInt32(OpCodes.Ldarg, i)
    static member LdArgA(i : int, il : IL) =
        match i with
        | s when s < 256 -> il.EmitByte(OpCodes.Ldarga_S, byte s)
        | i -> il.EmitInt32(OpCodes.Ldarga, i)
    static member StArg(i : int, il : IL) =
        match i with
        | s when s < 256 -> il.EmitByte(OpCodes.Starg_S, byte s)
        | i -> il.EmitInt32(OpCodes.Starg, i)

let inline zero (_ : 'x S) (_ : 'x S) (_ : IL) =
    ()

let inline pretend (_ : 'x S) (_ : 'y S) (_ : IL) = ()

////////////////////////////////////////
// Calls
////////////////////////////////////////

let inline call'x (meth : MethodInfo) (_ : 'x S) (_ : 'y S) (il : IL) =
    il.EmitMethod(OpCodes.Call, meth)

let inline call0 meth : Op<'x, 'x S> = call'x meth
let inline call1 meth : Op<'x S, 'x S> = call'x meth
let inline call2 meth : Op<'x S S, 'x S> = call'x meth
let inline call3 meth : Op<'x S S S, 'x S> = call'x meth
let inline call4 meth : Op<'x S S S S, 'x S> = call'x meth
let inline call5 meth : Op<'x S S S S S, 'x S> = call'x meth
let inline call6 meth : Op<'x S S S S S S, 'x S> = call'x meth

let inline call0'void meth : Op<'x, 'x> = call'x meth
let inline call1'void meth : Op<'x S, 'x> = call'x meth
let inline call2'void meth : Op<'x S S, 'x> = call'x meth
let inline call3'void meth : Op<'x S S S, 'x> = call'x meth
let inline call4'void meth : Op<'x S S S S, 'x> = call'x meth
let inline call5'void meth : Op<'x S S S S S, 'x> = call'x meth
let inline call6'void meth : Op<'x S S S S S S, 'x> = call'x meth

let inline calli'x (_ : 'x S S) (_ : 'y S) (il : IL) = il.Emit(OpCodes.Calli)
let inline calli0 (_ : 'x S S) (_: 'x S S) (il : IL) = il.Emit(OpCodes.Calli)
let inline calli1 (_ : 'x S S S) (_: 'x S S) (il : IL) = il.Emit(OpCodes.Calli)
let inline calli2 (_ : 'x S S S S) (_: 'x S S) (il : IL) = il.Emit(OpCodes.Calli)
let inline calli3 (_ : 'x S S S S S) (_: 'x S S) (il : IL) = il.Emit(OpCodes.Calli)
let inline calli4 (_ : 'x S S S S S S) (_: 'x S S) (il : IL) = il.Emit(OpCodes.Calli)
let inline calli5 (_ : 'x S S S S S S S) (_: 'x S S) (il : IL) = il.Emit(OpCodes.Calli)

let inline calli0'void (_ : 'x S S) (_: 'x S) (il : IL) = il.Emit(OpCodes.Calli)
let inline calli1'void (_ : 'x S S S) (_: 'x S) (il : IL) = il.Emit(OpCodes.Calli)
let inline calli2'void (_ : 'x S S S S) (_: 'x S) (il : IL) = il.Emit(OpCodes.Calli)
let inline calli3'void (_ : 'x S S S S S) (_: 'x S) (il : IL) = il.Emit(OpCodes.Calli)
let inline calli4'void (_ : 'x S S S S S S) (_: 'x S) (il : IL) = il.Emit(OpCodes.Calli)
let inline calli5'void (_ : 'x S S S S S S S) (_: 'x S) (il : IL) = il.Emit(OpCodes.Calli)

let inline callvirt'x (meth : MethodInfo) (_ : 'x S) (_ : 'y S) (il : IL) =
    il.EmitMethod(OpCodes.Callvirt, meth)

let inline callvirt1 meth : Op<'x S, 'x S> = callvirt'x meth
let inline callvirt2 meth : Op<'x S S, 'x S> = callvirt'x meth
let inline callvirt3 meth : Op<'x S S S, 'x S> = callvirt'x meth
let inline callvirt4 meth : Op<'x S S S S, 'x S> = callvirt'x meth
let inline callvirt5 meth : Op<'x S S S S S, 'x S> = callvirt'x meth

let inline callvirt1'void meth : Op<'x S, 'x> = callvirt'x meth
let inline callvirt2'void meth : Op<'x S S, 'x> = callvirt'x meth
let inline callvirt3'void meth : Op<'x S S S, 'x> = callvirt'x meth
let inline callvirt4'void meth : Op<'x S S S S, 'x> = callvirt'x meth
let inline callvirt5'void meth : Op<'x S S S S S, 'x> = callvirt'x meth

let inline newobj'x (cons : ConstructorInfo) (_ : 'x S) (_ : 'y S S) (il : IL) =
    il.EmitCtor(OpCodes.Newobj, cons)

let inline newobj0 cons : Op<'x, 'x S> = newobj'x cons
let inline newobj1 cons : Op<'x S, 'x S> = newobj'x cons
let inline newobj2 cons : Op<'x S S, 'x S> = newobj'x cons
let inline newobj3 cons : Op<'x S S S, 'x S> = newobj'x cons
let inline newobj4 cons : Op<'x S S S S, 'x S> = newobj'x cons
let inline newobj5 cons : Op<'x S S S S S, 'x S> = newobj'x cons

////////////////////////////////////////
// Primitive stack operations
////////////////////////////////////////

let inline nop (_ : 'x S) (_ : 'x S) (il : IL) = il.Emit(OpCodes.Nop)

let inline pop (_ : 'x S S) (_ : 'x S) (il : IL) = il.Emit(OpCodes.Pop)

let inline dup (_ : 'x S S) (_ : 'x S S S) (il : IL) = il.Emit(OpCodes.Dup)

let inline ret (_ : E S S) (_ : 'x S) (il : IL) = il.Emit(OpCodes.Ret)

let inline ret'void (_ : E S) (_ : 'x S) (il : IL) = il.Emit(OpCodes.Ret)

////////////////////////////////////////
// Loading constants
////////////////////////////////////////

let inline ldnull (_ : 'x S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Ldnull)

let inline ldc'i4 (i : int) (_ : 'x S) (_ : 'x S S) (il : IL) = CILHelpers.LdcI4(i, il)

let inline ldc'i8 (i : int64) (_ : 'x S) (_ : 'x S S) (il : IL) = il.EmitInt64(OpCodes.Ldc_I8, i)

let inline ldc'r4 (r : single) (_ : 'x S) (_ : 'x S S) (il : IL) = il.EmitSingle(OpCodes.Ldc_R4, r)

let inline ldc'r8 (r : double) (_ : 'x S) (_ : 'x S S) (il : IL) = il.EmitDouble(OpCodes.Ldc_R8, r)

let inline ldstr (s : string) (_ : 'x S) (_ : 'x S S) (il : IL) = il.EmitString(OpCodes.Ldstr, s)

////////////////////////////////////////
// Arithmetic and logic
////////////////////////////////////////

let inline add (_ : 'x S S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Add)

let inline add'ovf (_ : 'x S S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Add_Ovf)

let inline add'ovf'un (_ : 'x S S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Add_Ovf_Un)

let inline sub (_ : 'x S S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Sub)
let inline sub'ovf (_ : 'x S S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Sub_Ovf)
let inline sub'ovf'un (_ : 'x S S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Sub_Ovf_Un)

let inline mul (_ : 'x S S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Mul)
let inline mul'ovf (_ : 'x S S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Mul_Ovf)
let inline mul'ovf'un (_ : 'x S S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Mul_Ovf_Un)

let inline div (_ : 'x S S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Div)
let inline div'un (_ : 'x S S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Div_Un)

let inline rem (_ : 'x S S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Rem)
let inline rem'un (_ : 'x S S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Rem_Un)

let inline neg (_ : 'x S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Neg)

let inline shr (_ : 'x S S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Shr)
let inline shr'un (_ : 'x S S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Shr_Un)
let inline shl (_ : 'x S S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Shl)

let inline bit'and (_ : 'x S S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.And)
let inline bit'or (_ : 'x S S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Or)
let inline bit'xor (_ : 'x S S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Xor)
let inline bit'not (_ : 'x S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Not)

////////////////////////////////////////
// Comparison
////////////////////////////////////////

let inline ceq (_ : 'x S S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Ceq)
let inline cgt (_ : 'x S S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Cgt)
let inline cgt'un (_ : 'x S S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Cgt_Un)
let inline clt (_ : 'x S S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Clt)
let inline clt'un (_ : 'x S S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Clt_Un)

let inline ckfinite (_ : 'x S S) (_ : 'x S) (il : IL) = il.Emit(OpCodes.Ckfinite)

////////////////////////////////////////
// Conversion
////////////////////////////////////////

let inline conv'i (_ : 'x S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Conv_I)
let inline conv'i1 (_ : 'x S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Conv_I1)
let inline conv'i2 (_ : 'x S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Conv_I2)
let inline conv'i4 (_ : 'x S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Conv_I4)
let inline conv'i8 (_ : 'x S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Conv_I8)

let inline conv'ovf'i (_ : 'x S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Conv_Ovf_I)
let inline conv'ovf'i1 (_ : 'x S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Conv_Ovf_I1)
let inline conv'ovf'i2 (_ : 'x S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Conv_Ovf_I2)
let inline conv'ovf'i4 (_ : 'x S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Conv_Ovf_I4)
let inline conv'ovf'i8 (_ : 'x S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Conv_Ovf_I8)

let inline conv'ovf'i'un (_ : 'x S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Conv_Ovf_I_Un)
let inline conv'ovf'i1'un (_ : 'x S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Conv_Ovf_I1_Un)
let inline conv'ovf'i2'un (_ : 'x S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Conv_Ovf_I2_Un)
let inline conv'ovf'i4'un (_ : 'x S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Conv_Ovf_I4_Un)
let inline conv'ovf'i8'un (_ : 'x S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Conv_Ovf_I8_Un)

let inline conv'u (_ : 'x S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Conv_U)
let inline conv'u1 (_ : 'x S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Conv_U1)
let inline conv'u2 (_ : 'x S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Conv_U2)
let inline conv'u4 (_ : 'x S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Conv_U4)
let inline conv'u8 (_ : 'x S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Conv_U8)

let inline conv'ovf'u (_ : 'x S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Conv_Ovf_U)
let inline conv'ovf'u1 (_ : 'x S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Conv_Ovf_U1)
let inline conv'ovf'u2 (_ : 'x S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Conv_Ovf_U2)
let inline conv'ovf'u4 (_ : 'x S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Conv_Ovf_U4)
let inline conv'ovf'u8 (_ : 'x S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Conv_Ovf_U8)

let inline conv'ovf'u'un (_ : 'x S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Conv_Ovf_U_Un)
let inline conv'ovf'u1'un (_ : 'x S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Conv_Ovf_U1_Un)
let inline conv'ovf'u2'un (_ : 'x S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Conv_Ovf_U2_Un)
let inline conv'ovf'u4'un (_ : 'x S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Conv_Ovf_U4_Un)
let inline conv'ovf'u8'un (_ : 'x S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Conv_Ovf_U8_Un)

let inline conv'r4 (_ : 'x S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Conv_R4)
let inline conv'r'un (_ : 'x S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Conv_R_Un)
let inline conv'r8 (_ : 'x S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Conv_R8)

////////////////////////////////////////
// Arrays
////////////////////////////////////////

let inline newarr (elemTy : Type) (_ : 'x S S) (_ : 'x S S) (il : IL) = il.EmitTy(OpCodes.Newarr, elemTy)

let inline ldlen (_ : 'x S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Ldlen)

/// [array,index] -> [value]
let inline ldelem (elemTy : Type) (_ : 'x S S S) (_ : 'x S S) (il : IL) = il.EmitTy(OpCodes.Ldelem, elemTy)
let inline ldelem'i (_ : 'x S S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Ldelem_I)
let inline ldelem'i1 (_ : 'x S S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Ldelem_I1)
let inline ldelem'i2 (_ : 'x S S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Ldelem_I2)
let inline ldelem'i4 (_ : 'x S S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Ldelem_I4)
let inline ldelem'i8 (_ : 'x S S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Ldelem_I8)
let inline ldelem'u1 (_ : 'x S S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Ldelem_U1)
let inline ldelem'u2 (_ : 'x S S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Ldelem_U2)
let inline ldelem'u4 (_ : 'x S S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Ldelem_U4)
let inline ldelem'u8 (_ : 'x S S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Ldelem_I8)
let inline ldelem'r4 (_ : 'x S S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Ldelem_R4)
let inline ldelem'r8 (_ : 'x S S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Ldelem_R8)
let inline ldelem'ref (_ : 'x S S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Ldelem_Ref)
let inline ldelema (elemTy : Type) (_ : 'x S S S) (_ : 'x S S) (il : IL) = il.EmitTy(OpCodes.Ldelema, elemTy)

/// [array,index,value] -> []
let inline stelem (ty : Type) (_ : 'x S S S S) (_ : 'x S) (il : IL) = il.EmitTy(OpCodes.Stelem, ty)
let inline stelem'i (_ : 'x S S S S) (_ : 'x S) (il : IL) = il.Emit(OpCodes.Stelem_I)
let inline stelem'i1 (_ : 'x S S S S) (_ : 'x S) (il : IL) = il.Emit(OpCodes.Stelem_I1)
let inline stelem'i2 (_ : 'x S S S S) (_ : 'x S) (il : IL) = il.Emit(OpCodes.Stelem_I2)
let inline stelem'i4 (_ : 'x S S S S) (_ : 'x S) (il : IL) = il.Emit(OpCodes.Stelem_I4)
let inline stelem'i8 (_ : 'x S S S S) (_ : 'x S) (il : IL) = il.Emit(OpCodes.Stelem_I8)
let inline stelem'r4 (_ : 'x S S S S) (_ : 'x S) (il : IL) = il.Emit(OpCodes.Stelem_R4)
let inline stelem'r8 (_ : 'x S S S S) (_ : 'x S) (il : IL) = il.Emit(OpCodes.Stelem_R8)
let inline stelem'ref (_ : 'x S S S S) (_ : 'x S) (il : IL) = il.Emit(OpCodes.Stelem_Ref)

////////////////////////////////////////
// Arguments
////////////////////////////////////////

let inline ldarg i (_ : 'x S) (_ : 'x S S) (il : IL) = CILHelpers.LdArg(i, il)

let inline ldarga i (_ : 'x S) (_ : 'x S S) (il : IL) = CILHelpers.LdArgA(i, il)

let inline starg i (_ : 'x S S) (_ : 'x S) (il : IL) = CILHelpers.StArg(i, il)

////////////////////////////////////////
// Fields
////////////////////////////////////////

let inline stfld (field : FieldInfo) (_ : 'x S S S) (_ : 'x S) (il : IL) = il.EmitField(OpCodes.Stfld, field)
let inline stsfld (field : FieldInfo) (_ : 'x S S) (_ : 'x S) (il : IL) = il.EmitField(OpCodes.Stsfld, field)
let inline ldfld (field : FieldInfo) (_ : 'x S S) (_ : 'x S S) (il : IL) = il.EmitField(OpCodes.Ldfld, field)
let inline ldsfld (field : FieldInfo) (_ : 'x S) (_ : 'x S S) (il : IL) = il.EmitField(OpCodes.Ldsfld, field)
let inline ldflda (field : FieldInfo) (_ : 'x S S) (_ : 'x S S) (il : IL) = il.EmitField(OpCodes.Ldflda, field)
let inline ldsflda (field : FieldInfo) (_ : 'x S) (_ : 'x S S) (il : IL) = il.EmitField(OpCodes.Ldsflda, field)

////////////////////////////////////////
// Indirection
////////////////////////////////////////

let inline stind'i (_ : 'x S S S) (_ : 'x S) (il : IL) = il.Emit(OpCodes.Stind_I)
let inline stind'i1 (_ : 'x S S S) (_ : 'x S) (il : IL) = il.Emit(OpCodes.Stind_I1)
let inline stind'i2 (_ : 'x S S S) (_ : 'x S) (il : IL) = il.Emit(OpCodes.Stind_I2)
let inline stind'i4 (_ : 'x S S S) (_ : 'x S) (il : IL) = il.Emit(OpCodes.Stind_I4)
let inline stind'i8 (_ : 'x S S S) (_ : 'x S) (il : IL) = il.Emit(OpCodes.Stind_I8)
let inline stind'r4 (_ : 'x S S S) (_ : 'x S) (il : IL) = il.Emit(OpCodes.Stind_R4)
let inline stind'r8 (_ : 'x S S S) (_ : 'x S) (il : IL) = il.Emit(OpCodes.Stind_R8)
let inline stind'ref (_ : 'x S S S) (_ : 'x S) (il : IL) = il.Emit(OpCodes.Stind_Ref)

let inline ldind'i (_ : 'x S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Ldind_I)
let inline ldind'i1 (_ : 'x S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Ldind_I1)
let inline ldind'i2 (_ : 'x S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Ldind_I2)
let inline ldind'i4 (_ : 'x S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Ldind_I4)
let inline ldind'i8 (_ : 'x S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Ldind_I8)
let inline ldind'r4 (_ : 'x S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Ldind_R4)
let inline ldind'r8 (_ : 'x S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Ldind_R8)
let inline ldind'ref (_ : 'x S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Ldind_Ref)
let inline ldind'u1 (_ : 'x S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Ldind_U1)
let inline ldind'u2 (_ : 'x S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Ldind_U2)
let inline ldind'u4 (_ : 'x S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Ldind_U4)

////////////////////////////////////////
// Locals
////////////////////////////////////////

type LocalDefinition = internal | LocalDefinition of Type
type LocalTemporary = internal | LocalTemporary of Type

let deflocal ty = LocalDefinition ty
let tmplocal ty = LocalTemporary ty
    
let inline ldloc (local : LocalBuilder) (_ : 'x S) (_ : 'x S S) (il : IL) = CILHelpers.LdLoc(local, il)

let inline ldloca (local : LocalBuilder) (_ : 'x S) (_ : 'x S S) (il : IL) = CILHelpers.LdLoca(local, il)
    
let inline stloc (local : LocalBuilder) (_ : 'x S S) (_ : 'x S) (il : IL) = CILHelpers.StLoc(local, il)

////////////////////////////////////////
// Labels and branching
////////////////////////////////////////

type Label<'stack> = Label of Label

type LabelDefinition = internal | LabelDefinition

let deflabel = LabelDefinition

let inline mark (Label lbl : 'x Label) (_ : 'x S) (_ : 'x S) (il : IL) = il.MarkLabel(lbl)

// don't enforce anything about the following stack
// since we're jumping away unconditionally
let inline br (Label lbl : 'x Label) (_ : 'x S) (_ : _ S) (il : IL) =
    il.EmitLabel(OpCodes.Br, lbl)

let inline br's (Label lbl : 'x Label) (_ : 'x S) (_ : _ S) (il : IL) =
    il.EmitLabel(OpCodes.Br_S, lbl)

let inline leave (Label lbl : E Label) (_ : 'x S) (_ : _ S) (il : IL) =
    il.EmitLabel(OpCodes.Leave, lbl)

let inline leave's (Label lbl : E Label) (_ : 'x S) (_ : _ S) (il : IL) =
    il.EmitLabel(OpCodes.Leave, lbl)

let inline brtrue (Label lbl : 'x Label) (_ : 'x S S) (_: 'x S) (il : IL) = il.EmitLabel(OpCodes.Brtrue, lbl)
let inline brtrue's (Label lbl : 'x Label) (_ : 'x S S) (_: 'x S) (il : IL) = il.EmitLabel(OpCodes.Brtrue_S, lbl)

let inline brfalse (Label lbl : 'x Label) (_ : 'x S S) (_: 'x S) (il : IL) = il.EmitLabel(OpCodes.Brfalse, lbl)
let inline brfalse's (Label lbl : 'x Label) (_ : 'x S S) (_: 'x S) (il : IL) = il.EmitLabel(OpCodes.Brfalse_S, lbl)

let inline beq (Label lbl : 'x Label) (_ : 'x S S S) (_: 'x S) (il : IL) = il.EmitLabel(OpCodes.Beq, lbl)
let inline beq's (Label lbl : 'x Label) (_ : 'x S S S) (_: 'x S) (il : IL) = il.EmitLabel(OpCodes.Beq_S, lbl)

let inline bne'un (Label lbl : 'x Label) (_ : 'x S S S) (_: 'x S) (il : IL) = il.EmitLabel(OpCodes.Bne_Un, lbl)
let inline bne'un's (Label lbl : 'x Label) (_ : 'x S S S) (_: 'x S) (il : IL) = il.EmitLabel(OpCodes.Bne_Un_S, lbl)

let inline bge (Label lbl : 'x Label) (_ : 'x S S S) (_: 'x S) (il : IL) = il.EmitLabel(OpCodes.Bge, lbl)
let inline bge's (Label lbl : 'x Label) (_ : 'x S S S) (_: 'x S) (il : IL) = il.EmitLabel(OpCodes.Bge_S, lbl)
let inline bge'un (Label lbl : 'x Label) (_ : 'x S S S) (_: 'x S) (il : IL) = il.EmitLabel(OpCodes.Bge_Un, lbl)
let inline bge'un's (Label lbl : 'x Label) (_ : 'x S S S) (_: 'x S) (il : IL) = il.EmitLabel(OpCodes.Bge_Un_S, lbl)

let inline ble (Label lbl : 'x Label) (_ : 'x S S S) (_: 'x S) (il : IL) = il.EmitLabel(OpCodes.Ble, lbl)
let inline ble's (Label lbl : 'x Label) (_ : 'x S S S) (_: 'x S) (il : IL) = il.EmitLabel(OpCodes.Ble_S, lbl)
let inline ble'un (Label lbl : 'x Label) (_ : 'x S S S) (_: 'x S) (il : IL) = il.EmitLabel(OpCodes.Ble_Un, lbl)
let inline ble'un's (Label lbl : 'x Label) (_ : 'x S S S) (_: 'x S) (il : IL) = il.EmitLabel(OpCodes.Ble_Un_S, lbl)

let inline bgt (Label lbl : 'x Label) (_ : 'x S S S) (_: 'x S) (il : IL) = il.EmitLabel(OpCodes.Bgt, lbl)
let inline bgt's (Label lbl : 'x Label) (_ : 'x S S S) (_: 'x S) (il : IL) = il.EmitLabel(OpCodes.Bgt_S, lbl)
let inline bgt'un (Label lbl : 'x Label) (_ : 'x S S S) (_: 'x S) (il : IL) = il.EmitLabel(OpCodes.Bgt_Un, lbl)
let inline bgt'un's (Label lbl : 'x Label) (_ : 'x S S S) (_: 'x S) (il : IL) = il.EmitLabel(OpCodes.Bgt_Un_S, lbl)

let inline blt (Label lbl : 'x Label) (_ : 'x S S S) (_: 'x S) (il : IL) = il.EmitLabel(OpCodes.Blt, lbl)
let inline blt's (Label lbl : 'x Label) (_ : 'x S S S) (_: 'x S) (il : IL) = il.EmitLabel(OpCodes.Blt_S, lbl)
let inline blt'un (Label lbl : 'x Label) (_ : 'x S S S) (_: 'x S) (il : IL) = il.EmitLabel(OpCodes.Blt_Un, lbl)
let inline blt'un's (Label lbl : 'x Label) (_ : 'x S S S) (_: 'x S) (il : IL) = il.EmitLabel(OpCodes.Blt_Un_S, lbl)

let inline switch (labels : 'x Label seq) (_ : 'x S S) (_ : 'x S) (il : IL) =
    let labels = [| for Label lbl in labels -> lbl |]
    il.EmitSwitch(labels)

////////////////////////////////////////
// Type system
////////////////////////////////////////

let inline ldtoken (ty : Type) (_ : 'x S) (_ : 'x S S) (il : IL) = il.EmitTy(OpCodes.Ldtoken, ty)
let inline ldftn (meth : MethodInfo) (_ : 'x S) (_ : 'x S S) (il : IL) = il.EmitMethod(OpCodes.Ldftn, meth)
let inline ldvirtftn (meth : MethodInfo) (_ : 'x S S) (_ : 'x S S) (il : IL) = il.EmitMethod(OpCodes.Ldvirtftn, meth)
let inline box'val (ty : Type) (_ : 'x S S) (_ : 'x S S) (il : IL) = il.EmitTy(OpCodes.Box, ty)
let inline unbox'any (ty : Type) (_ : 'x S S) (_ : 'x S S) (il : IL) = il.EmitTy(OpCodes.Unbox_Any, ty)
let inline unbox'val (ty : Type) (_ : 'x S S) (_ : 'x S S) (il : IL) = il.EmitTy(OpCodes.Unbox, ty)
let inline castclass (ty : Type) (_ : 'x S S) (_ : 'x S S) (il : IL) = il.EmitTy(OpCodes.Castclass, ty)
let inline isinst (ty : Type) (_ : 'x S S) (_ : 'x S S) (il : IL) = il.EmitTy(OpCodes.Isinst, ty)
let inline refanyval (ty : Type) (_ : 'x S S) (_ : 'x S S) (il : IL) = il.EmitTy(OpCodes.Refanyval, ty)
let inline refanytype (_ : 'x S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Refanytype)
let inline initobj (ty : Type) (_ : 'x S S) (_ : 'x S) (il : IL) = il.EmitTy(OpCodes.Initobj, ty)
let inline mkrefany (ty : Type) (_ : 'x S S) (_ : 'x S S) (il : IL) = il.EmitTy(OpCodes.Mkrefany, ty)

////////////////////////////////////////
// Exception stuff
////////////////////////////////////////

let inline endfilter (_ : 'x S S) (_ : 'x S) (il : IL) = il.Emit(OpCodes.Endfilter)
let inline endfinally (_ : 'x S) (_ : 'x S) (il : IL) = il.Emit(OpCodes.Endfinally)
let inline rethrow (_ : 'x S) (_ : 'x S) (il : IL) = il.Emit(OpCodes.Rethrow)
let inline throw (_ : 'x S S) (_ : _ S) (il : IL) = il.Emit(OpCodes.Throw)

////////////////////////////////////////
// Weird stuff and memory operations
////////////////////////////////////////

let inline arglist (_ : 'x S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Arglist)
let inline cpblk (_ : 'x S S S S) (_ : 'x S) (il : IL) = il.Emit(OpCodes.Cpblk)
let inline stobj (ty : Type) (_ : 'x S S S) (_ : 'x S) (il : IL) = il.EmitTy(OpCodes.Stobj, ty)
let inline ldobj (ty : Type) (_ : 'x S S) (_ : 'x S S) (il : IL) = il.EmitTy(OpCodes.Ldobj, ty)
let inline cpobj (ty : Type) (_ : 'x S S S) (_ : 'x S) (il : IL) = il.EmitTy(OpCodes.Cpobj, ty)
let inline sizeof (ty : Type) (_ : 'x S) (_ : 'x S S) (il : IL) = il.EmitTy(OpCodes.Sizeof, ty)
let inline jmp (meth : MethodInfo) (_ : E S) (_ : E S) (il : IL) = il.EmitMethod(OpCodes.Jmp, meth)
let inline localloc (_ : 'x S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Localloc)
let inline initblk (_ : 'x S S S S) (_ : 'x S) (il : IL) = il.Emit(OpCodes.Initblk)

////////////////////////////////////////
// Prefixes
////////////////////////////////////////

let inline constrained (ty : Type) (_ : 'x S) (_ : 'x S) (il : IL) = il.EmitTy(OpCodes.Constrained, ty)
let inline tail (_ : 'x S) (_ : 'x S) (il : IL) = il.Emit(OpCodes.Tailcall)
let inline unaligned (_ : 'x S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Unaligned)
let inline volatile' (_ : 'x S S) (_ : 'x S S) (il : IL) = il.Emit(OpCodes.Volatile)