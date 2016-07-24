module LicenseToCIL.Op
open LicenseToCIL
open LicenseToCIL.Stack
open FSharp.Quotations
open System
open System.Reflection
open System.Reflection.Emit

type Local = LocalBuilder
type Label<'stack> = Label of Label

type LabelDefinition = internal | LabelDefinition

val deflabel : LabelDefinition
val mark : 'x Label -> Op<'x, 'x>

val call'x : MethodInfo -> Op<'x, 'y>
val call0 : MethodInfo -> Op<'x, 'x S> 
val call1 : MethodInfo -> Op<'x S, 'x S>
val call2 : MethodInfo -> Op<'x S S, 'x S>
val call3 : MethodInfo -> Op<'x S S S, 'x S>
val call4 : MethodInfo -> Op<'x S S S S, 'x S>
val call5 : MethodInfo -> Op<'x S S S S S, 'x S>

val call0'void : MethodInfo -> Op<'x, 'x> 
val call1'void : MethodInfo -> Op<'x S, 'x>
val call2'void : MethodInfo -> Op<'x S S, 'x>
val call3'void : MethodInfo -> Op<'x S S S, 'x>
val call4'void : MethodInfo -> Op<'x S S S S, 'x>
val call5'void : MethodInfo -> Op<'x S S S S S, 'x>

val calli'x : Op<'x S, 'y>
val calli0 : Op<'x S, 'x S>
val calli1 : Op<'x S S, 'x S>
val calli2 : Op<'x S S S, 'x S>
val calli3 : Op<'x S S S S, 'x S>
val calli4 : Op<'x S S S S S, 'x S>
val calli5 : Op<'x S S S S S S, 'x S>

val calli'void : Op<'x S, 'y>
val calli0'void : Op<'x S, 'x S>
val calli1'void : Op<'x S S, 'x S>
val calli2'void : Op<'x S S S, 'x S>
val calli3'void : Op<'x S S S S, 'x S>
val calli4'void : Op<'x S S S S S, 'x S>
val calli5'void : Op<'x S S S S S S, 'x S>

val callvirt'x : MethodInfo -> Op<'x, 'y>
val callvirt0 : MethodInfo -> Op<'x, 'x S> 
val callvirt1 : MethodInfo -> Op<'x S, 'x S>
val callvirt2 : MethodInfo -> Op<'x S S, 'x S>
val callvirt3 : MethodInfo -> Op<'x S S S, 'x S>
val callvirt4 : MethodInfo -> Op<'x S S S S, 'x S>
val callvirt5 : MethodInfo -> Op<'x S S S S S, 'x S>

val callvirt0'void : MethodInfo -> Op<'x, 'x> 
val callvirt1'void : MethodInfo -> Op<'x S, 'x>
val callvirt2'void : MethodInfo -> Op<'x S S, 'x>
val callvirt3'void : MethodInfo -> Op<'x S S S, 'x>
val callvirt4'void : MethodInfo -> Op<'x S S S S, 'x>
val callvirt5'void : MethodInfo -> Op<'x S S S S S, 'x>

val newobj'x : ConstructorInfo -> Op<'x, 'y S>
val newobj0 : ConstructorInfo -> Op<'x, 'x S>
val newobj1 : ConstructorInfo -> Op<'x S, 'x S>
val newobj2 : ConstructorInfo -> Op<'x S S, 'x S>
val newobj3 : ConstructorInfo -> Op<'x S S S, 'x S>
val newobj4 : ConstructorInfo -> Op<'x S S S S, 'x S>
val newobj5 : ConstructorInfo -> Op<'x S S S S S, 'x S>

val add : Op<'x S S, 'x S>
val add'ovf : Op<'x S S, 'x S>
val add'ovf'un : Op<'x S S, 'x S>
val arglist : Op<'x, 'x S>
val beq : 'x Label -> Op<'x S S, 'x>
val beq's : 'x Label -> Op<'x S S, 'x>
val bge : 'x Label -> Op<'x S S, 'x>
val bge's : 'x Label -> Op<'x S S, 'x>
val bge'un : 'x Label -> Op<'x S S, 'x>
val bge'un's : 'x Label -> Op<'x S S, 'x>
val bgt : 'x Label -> Op<'x S S, 'x>
val bgt's : 'x Label -> Op<'x S S, 'x>
val bgt'un : 'x Label -> Op<'x S S, 'x>
val bgt'un's : 'x Label -> Op<'x S S, 'x>
val bit'and : Op<'x S S, 'x S>
val bit'not : Op<'x S, 'x S>
val bit'or : Op<'x S S, 'x S>
val bit'xor : Op<'x S S, 'x S>
val ble : 'x Label -> Op<'x S S, 'x>
val ble's : 'x Label -> Op<'x S S, 'x>
val ble'un : 'x Label -> Op<'x S S, 'x>
val ble'un's : 'x Label -> Op<'x S S, 'x>
val blt : 'x Label -> Op<'x S S, 'x>
val blt's : 'x Label -> Op<'x S S, 'x>
val blt'un : 'x Label -> Op<'x S S, 'x>
val blt'un's : 'x Label -> Op<'x S S, 'x>
val bne'un : 'x Label -> Op<'x S S, 'x>
val bne'un's : 'x Label -> Op<'x S S, 'x>
val box'val : valueTy : Type -> Op<'x S, 'x S>
val br : 'x Label -> Op<'x, 'y>
val br's : 'x Label -> Op<'x, 'y>
val brfalse : 'x Label -> Op<'x S, 'x>
val brfalse's : 'x Label -> Op<'x S, 'x>
val brtrue : 'x Label -> Op<'x S, 'x>
val brtrue's : 'x Label -> Op<'x S, 'x>
val castclass : toTy : Type -> Op<'x S, 'x S>
val ceq : Op<'x S S, 'x S>
val cgt : Op<'x S S, 'x S>
val cgt'un : Op<'x S S, 'x S>
val ckfinite : Op<'x S, 'x>
val clt : Op<'x S S, 'x S>
val clt'un : Op<'x S S, 'x S>
val constrained : ty : Type -> Op<'x, 'x>
val constrained : ty : Type -> Op<'x, 'x>
val conv'i : Op<'x S, 'x S>
val conv'i1 : Op<'x S, 'x S>
val conv'i2 : Op<'x S, 'x S>
val conv'i4 : Op<'x S, 'x S>
val conv'i8 : Op<'x S, 'x S>
val conv'ovf'i : Op<'x S, 'x S>
val conv'ovf'i'un : Op<'x S, 'x S>
val conv'ovf'i1 : Op<'x S, 'x S>
val conv'ovf'i1'un : Op<'x S, 'x S>
val conv'ovf'i2 : Op<'x S, 'x S>
val conv'ovf'i2'un : Op<'x S, 'x S>
val conv'ovf'i4 : Op<'x S, 'x S>
val conv'ovf'i4'un : Op<'x S, 'x S>
val conv'ovf'i8 : Op<'x S, 'x S>
val conv'ovf'i8'un : Op<'x S, 'x S>
val conv'ovf'u : Op<'x S, 'x S>
val conv'ovf'u'un : Op<'x S, 'x S>
val conv'ovf'u1 : Op<'x S, 'x S>
val conv'ovf'u1'un : Op<'x S, 'x S>
val conv'ovf'u2 : Op<'x S, 'x S>
val conv'ovf'u2'un : Op<'x S, 'x S>
val conv'ovf'u4 : Op<'x S, 'x S>
val conv'ovf'u4'un : Op<'x S, 'x S>
val conv'ovf'u8 : Op<'x S, 'x S>
val conv'ovf'u8'un : Op<'x S, 'x S>
val conv'r'un : Op<'x S, 'x S>
val conv'r4 : Op<'x S, 'x S>
val conv'r8 : Op<'x S, 'x S>
val conv'u : Op<'x S, 'x S>
val conv'u1 : Op<'x S, 'x S>
val conv'u2 : Op<'x S, 'x S>
val conv'u4 : Op<'x S, 'x S>
val conv'u8 : Op<'x S, 'x S>
val cpblk : Op<'x S S S, 'x>
val cpobj : ty : Type -> Op<'x S S, 'x>
val div : Op<'x S S, 'x S>
val div'un : Op<'x S S, 'x S>
val dup : Op<'x S, 'x S S>
val endfilter : Op<'x S, 'x>
val endfinally : Op<'x, 'x>
val initblk : Op<'x S S S, 'x>
val initobj : ty : Type -> Op<'x S, 'x>
val isinst : ty : Type -> Op<'x S, 'x S>
val jmp : MethodInfo -> Op<E, E>
val ldarg : int -> Op<'x, 'x S>
val ldarga : int -> Op<'x, 'x S>
val ldc'i4 : int -> Op<'x, 'x S>
val ldc'i8 : int64 -> Op<'x, 'x S>
val ldc'r4 : single -> Op<'x, 'x S>
val ldc'r8 : double -> Op<'x, 'x S>
val ldelem : elemTy : Type -> Op<'x S S, 'x S>
val ldelem'i : Op<'x S S, 'x S>
val ldelem'i1 : Op<'x S S, 'x S>
val ldelem'i2 : Op<'x S S, 'x S>
val ldelem'i4 : Op<'x S S, 'x S>
val ldelem'i8 : Op<'x S S, 'x S>
val ldelem'r4 : Op<'x S S, 'x S>
val ldelem'r8 : Op<'x S S, 'x S>
val ldelem'ref : Op<'x S S, 'x S>
val ldelem'u1 : Op<'x S S, 'x S>
val ldelem'u2 : Op<'x S S, 'x S>
val ldelem'u4 : Op<'x S S, 'x S>
val ldelema : ty : Type -> Op<'x S S, 'x S>
val ldfld : FieldInfo -> Op<'x S, 'x S>
val ldflda : FieldInfo -> Op<'x S, 'x S>
val ldftn : MethodInfo -> Op<'x, 'x S>
val ldind'i : Op<'x S, 'x S>
val ldind'i1 : Op<'x S, 'x S>
val ldind'i2 : Op<'x S, 'x S>
val ldind'i4 : Op<'x S, 'x S>
val ldind'i8 : Op<'x S, 'x S>
val ldind'r4 : Op<'x S, 'x S>
val ldind'r8 : Op<'x S, 'x S>
val ldind'ref : Op<'x S, 'x S>
val ldind'u1 : Op<'x S, 'x S>
val ldind'u2 : Op<'x S, 'x S>
val ldind'u4 : Op<'x S, 'x S>
val ldlen : Op<'x S, 'x S>
val ldloc : Local -> Op<'x, 'x S>
val ldloca : Local -> Op<'x, 'x S>
val ldnull : Op<'x, 'x S>
val ldobj : ty : Type -> Op<'x S, 'x S>
val ldsfld : FieldInfo -> Op<'x, 'x S>
val ldsflda : FieldInfo -> Op<'x, 'x S>
val ldstr : str : string -> Op<'x, 'x S>
val ldtoken : ty : Type -> Op<'x, 'x S>
val ldvirtftn : MethodInfo -> Op<'x S, 'x S>
val leave : E Label -> Op<'y, 'z>
val leave's : E Label -> Op<'y, 'z>
val localloc : Op<'x S, 'x S>
val mkrefany : ty : Type -> Op<'x S, 'x S>
val mul : Op<'x S S, 'x S>
val mul'ovf : Op<'x S S, 'x S>
val mul'ovf'un : Op<'x S S, 'x S>
val neg : Op<'x S, 'x S>
val newarr : elemTy : Type -> Op<'x S, 'x S>
val nop : Op<'x, 'x>
val pop : Op<'x S, 'x>
val refanytype : Op<'x S, 'x S>
val refanyval : ty : Type -> Op<'x S, 'x S>
val rem : Op<'x S S, 'x S>
val rem'un : Op<'x S S, 'x S>
val ret : Op<E S, E>
val ret'void : Op<E, E>
val rethrow : Op<'x, 'x>
val shl : Op<'x S S, 'x S>
val shr : Op<'x S S, 'x S>
val shr'un : Op<'x S S, 'x S>
val sizeof : valueTy : Type -> Op<'x, 'x S>
val starg : int -> Op<'x S, 'x>
val stelem : elemTy : Type -> Op<'x S S S, 'x>
val stelem'i : Op<'x S S S, 'x>
val stelem'i1 : Op<'x S S S, 'x>
val stelem'i2 : Op<'x S S S, 'x>
val stelem'i4 : Op<'x S S S, 'x>
val stelem'i8 : Op<'x S S S, 'x>
val stelem'r4 : Op<'x S S S, 'x>
val stelem'r8 : Op<'x S S S, 'x>
val stelem'ref : Op<'x S S S, 'x>
val stfld : FieldInfo -> Op<'x S S, 'x>
val stind'i : Op<'x S S, 'x>
val stind'i1 : Op<'x S S, 'x>
val stind'i2 : Op<'x S S, 'x>
val stind'i4 : Op<'x S S, 'x>
val stind'i8 : Op<'x S S, 'x>
val stind'r4 : Op<'x S S, 'x>
val stind'r8 : Op<'x S S, 'x>
val stind'ref : Op<'x S S, 'x>
val stloc : Local -> Op<'x S, 'x>
val stobj : Type -> Op<'x S S, 'x>
val stsfld : FieldInfo -> Op<'x S, 'x>
val sub : Op<'x S S, 'x S>
val sub'ovf : Op<'x S S, 'x S>
val sub'ovf'un : Op<'x S S, 'x S>
val switch : 'x Label seq -> Op<'x S, 'x>
val tail : Op<'x, 'x>
val throw : Op<'x S, 'y>
val unbox'val : ty : Type -> Op<'x S, 'x S>
val unbox'any : ty : Type -> Op<'x S, 'x S>


