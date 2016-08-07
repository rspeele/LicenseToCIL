module LicenseToCIL.Ops
open LicenseToCIL
open LicenseToCIL.Stack
open FSharp.Quotations
open System
open System.Reflection
open System.Reflection.Emit

type Local = LocalBuilder
type Label<'stack> = Label of Label

type LabelDefinition = internal | LabelDefinition
type LocalDefinition = internal | LocalDefinition of Type
type LocalTemporary = internal | LocalTemporary of Type

/// Sequence together two stack operations.
val inline combine : Op<'i, 'm> -> (unit -> Op<'m, 'e>) -> Op<'i, 'e>
/// Emit no instructions and retain stack state.
val inline zero : Op<'x, 'x>
/// Force the stack state to appear as desired to the type checker.
val inline pretend<'x, 'y> : S<'x> -> IL -> S<'y>

/// Define a new local, e.g. `let! loc = deflocal typeof<int>`.
val deflocal : Type -> LocalDefinition
/// Obtain a temporary local, e.g. `let! loc = tmplocal typeof<int>`.
/// The local may contain an arbitrary previous value.
val tmplocal : Type -> LocalTemporary
/// Define a new label, e.g. `let! lbl = deflabel`.
val deflabel : LabelDefinition

/// Mark the position of a label.
val mark : 'x Label -> Op<'x, 'x>

/// [? -> ?] Call method taking an unknown number of arguments.
/// Not recommended -- favor exact variants.
val call'x : MethodInfo -> Op<'x, 'y>
/// [_ -> _, value] Call method taking zero arguments and returning a value.
val call0 : MethodInfo -> Op<'x, 'x S> 
/// [_, arg0 -> _, value] Call method taking 1 argument and returning a value.
val call1 : MethodInfo -> Op<'x S, 'x S>
/// [_, arg0, arg1 -> _, value] Call method taking 2 arguments and returning a value.
val call2 : MethodInfo -> Op<'x S S, 'x S>
/// [_, arg0, arg1, arg2 -> _, value] Call method taking 3 arguments and returning a value.
val call3 : MethodInfo -> Op<'x S S S, 'x S>
/// [_, arg0, arg1. arg2, arg3 -> _, value] Call method taking 4 arguments and returning a value.
val call4 : MethodInfo -> Op<'x S S S S, 'x S>
/// [_, arg0, arg1, arg2, arg3, arg4 -> _, value] Call method taking 5 arguments and returning a value.
val call5 : MethodInfo -> Op<'x S S S S S, 'x S>

/// [_ -> _] Call void method taking zero arguments.
val call0'void : MethodInfo -> Op<'x, 'x> 
/// [_, arg0 -> _] Call void method taking 1 argument.
val call1'void : MethodInfo -> Op<'x S, 'x>
/// [_, arg0, arg1 -> _] Call void method taking 2 arguments.
val call2'void : MethodInfo -> Op<'x S S, 'x>
/// [_, arg0, arg1, arg2 -> _] Call void method taking 3 arguments.
val call3'void : MethodInfo -> Op<'x S S S, 'x>
/// [_, arg0, arg1, arg2, arg3 -> _] Call void method taking 4 arguments.
val call4'void : MethodInfo -> Op<'x S S S S, 'x>
/// [_, arg0, arg1, arg2, arg3, arg4 -> _] Call void method taking 5 arguments.
val call5'void : MethodInfo -> Op<'x S S S S S, 'x>

/// [?, ptr -> ?] Indirectly call method taking an unknown number of arguments.
/// Not recommended -- favor exact variants.
val calli'x : Op<'x S, 'y>
/// [_, ptr -> _, value] Indirectly call method taking zero arguments and returning a value.
val calli0 : Op<'x S, 'x S>
/// [_, arg0, ptr -> _, value] Indirectly call method taking 1 argument and returning a value.
val calli1 : Op<'x S S, 'x S>
/// [_, arg0, arg1, ptr -> _, value] Indirectly call method taking 2 arguments and returning a value.
val calli2 : Op<'x S S S, 'x S>
/// [_, arg0, arg1, arg2, ptr -> _, value] Indirectly call method taking 3 arguments and returning a value.
val calli3 : Op<'x S S S S, 'x S>
/// [_, arg0, arg1, arg2, arg3, ptr -> _, value] Indirectly call method taking 4 arguments and returning a value.
val calli4 : Op<'x S S S S S, 'x S>
/// [_, arg0, arg1, arg2, arg3, arg4, ptr -> _, value] Indirectly call method taking 5 arguments and returning a value.
val calli5 : Op<'x S S S S S S, 'x S>

/// [_, ptr -> _] Indirectly call void method taking zero arguments.
val calli0'void : Op<'x S, 'x S>
/// [_, arg0, ptr -> _] Indirectly call void method taking 1 argument.
val calli1'void : Op<'x S S, 'x S>
/// [_, arg0, arg1, ptr -> _] Indirectly call void method taking 2 arguments.
val calli2'void : Op<'x S S S, 'x S>
/// [_, arg0, arg1, arg2, ptr -> _] Indirectly call void method taking 3 arguments.
val calli3'void : Op<'x S S S S, 'x S>
/// [_, arg0, arg1, arg2, arg3, ptr -> _] Indirectly call void method taking 4 arguments.
val calli4'void : Op<'x S S S S S, 'x S>
/// [_, arg0, arg1, arg2, arg3, arg4, ptr -> _] Indirectly call void method taking 5 arguments.
val calli5'void : Op<'x S S S S S S, 'x S>

/// [? -> ?] Indirectly call virtual method taking an unknown number of arguments.
/// Not recommended -- favor exact variants.
val callvirt'x : MethodInfo -> Op<'x, 'y>
/// [_, obj -> _, value] Call virtual method taking 1 (instance) argument and returning a value.
val callvirt1 : MethodInfo -> Op<'x S, 'x S>
/// [_, obj, arg0 -> _, value] Call virtual method taking 2 arguments and returning a value.
val callvirt2 : MethodInfo -> Op<'x S S, 'x S>
/// [_, obj, arg0, arg1 -> _, value] Call virtual method taking 3 arguments and returning a value.
val callvirt3 : MethodInfo -> Op<'x S S S, 'x S>
/// [_, obj, arg0, arg1, arg2 -> _, value] Call virtual method taking 4 arguments and returning a value.
val callvirt4 : MethodInfo -> Op<'x S S S S, 'x S>
/// [_, obj, arg0, arg1, arg2, arg3 -> _, value] Call virtual method taking 5 arguments and returning a value.
val callvirt5 : MethodInfo -> Op<'x S S S S S, 'x S>

/// [_, obj -> _] Call virtual void method taking 1 (instance) argument.
val callvirt1'void : MethodInfo -> Op<'x S, 'x>
/// [_, obj, arg0 -> _] Call virtual void method taking 2 arguments.
val callvirt2'void : MethodInfo -> Op<'x S S, 'x>
/// [_, obj, arg0, arg1 -> _] Call virtual void method taking 3 arguments.
val callvirt3'void : MethodInfo -> Op<'x S S S, 'x>
/// [_, obj, arg0, arg1, arg2 -> _] Call virtual void method taking 4 arguments.
val callvirt4'void : MethodInfo -> Op<'x S S S S, 'x>
/// [_, obj, arg0, arg1, arg2, arg3 -> _] Call virtual void method taking 5 arguments.
val callvirt5'void : MethodInfo -> Op<'x S S S S S, 'x>

/// [? -> ?] Call constructor taking an unknown number of arguments.
/// Not recommended -- favor exact variants.
val newobj'x : ConstructorInfo -> Op<'x, 'y S>
/// [_ -> _, object] Call constructor taking zero arguments.
val newobj0 : ConstructorInfo -> Op<'x, 'x S>
/// [_, arg0 -> _, object] Call constructor taking 1 argument.
val newobj1 : ConstructorInfo -> Op<'x S, 'x S>
/// [_, arg0, arg1 -> _, object] Call constructor taking 2 arguments.
val newobj2 : ConstructorInfo -> Op<'x S S, 'x S>
/// [_, arg0, arg1, arg2 -> _, object] Call constructor taking 3 arguments.
val newobj3 : ConstructorInfo -> Op<'x S S S, 'x S>
/// [_, arg0, arg1, arg2, arg3 -> _, object] Call constructor taking 4 arguments.
val newobj4 : ConstructorInfo -> Op<'x S S S S, 'x S>
/// [_, arg0, arg1, arg2, arg3, arg4 -> _, object] Call constructor taking 5 arguments.
val newobj5 : ConstructorInfo -> Op<'x S S S S S, 'x S>

/// [_, value1, value2 --> _, result] Add two values, returning a new value.
val add : Op<'x S S, 'x S>
/// [_, value1, value2 --> _, result] Add signed integer values with overflow check.
val add'ovf : Op<'x S S, 'x S>
/// [_, value1, value2 --> _, result] Add unsigned integer values with overflow check.
val add'ovf'un : Op<'x S S, 'x S>
/// [_ --> _, argListHandle] Return argument list handle for the current method.
val arglist : Op<'x, 'x S>
/// [_, value1, value2 --> _] Branch to target if equal.
val beq : target : 'x Label -> Op<'x S S, 'x>
/// [_, value1, value2 --> _] Branch to target if equal, short form.
val beq's : target : 'x Label -> Op<'x S S, 'x>
/// [_, value1, value2 --> _] Branch to target if greater than or equal to.
val bge : target : 'x Label -> Op<'x S S, 'x>
/// [_, value1, value2 --> _] Branch to target if greater than or equal to, short form.
val bge's : target : 'x Label -> Op<'x S S, 'x>
/// [_, value1, value2 --> _] Branch to target if greater than or equal to (unsigned or unordered).
val bge'un : target : 'x Label -> Op<'x S S, 'x>
/// [_, value1, value2 --> _] Branch to target if greater than or equal to (unsigned or unordered), short form.
val bge'un's : target : 'x Label -> Op<'x S S, 'x>
/// [_, value1, value2 --> _] Branch to target if greater than.
val bgt : target : 'x Label -> Op<'x S S, 'x>
/// [_, value1, value2 --> _] Branch to target if greater than, short form.
val bgt's : target : 'x Label -> Op<'x S S, 'x>
/// [_, value1, value2 --> _] Branch to target if greater than (unsigned or unordered).
val bgt'un : target : 'x Label -> Op<'x S S, 'x>
/// [_, value1, value2 --> _] Branch to target if greater than (unsigned or unordered), short form.
val bgt'un's : target : 'x Label -> Op<'x S S, 'x>
/// [_, value1, value2 --> _, result] Bitwise AND of two integer values, returns an integer.
val bit'and : Op<'x S S, 'x S>
/// [_, value --> _, result] Bitwise complement of an integer value.
val bit'not : Op<'x S, 'x S>
/// [_, value1, value2 --> _, result] Bitwise OR of two integer values, returns an integer.
val bit'or : Op<'x S S, 'x S>
/// [_, value1, value2 --> _, result] Bitwise XOR of integer values, returns an integer.
val bit'xor : Op<'x S S, 'x S>
/// [_, value1, value2 --> _] Branch to target if less than or equal to.
val ble : target : 'x Label -> Op<'x S S, 'x>
/// [_, value1, value2 --> _] Branch to target if less than or equal to, short form.
val ble's : target : 'x Label -> Op<'x S S, 'x>
/// [_, value1, value2 --> _] Branch to target if less than or equal to (unsigned or unordered).
val ble'un : target : 'x Label -> Op<'x S S, 'x>
/// [_, value1, value2 --> _] Branch to target if less than or equal to (unsigned or unordered), short form.
val ble'un's : target : 'x Label -> Op<'x S S, 'x>
/// [_, value1, value2 --> _] Branch to target if less than.
val blt : target : 'x Label -> Op<'x S S, 'x>
/// [_, value1, value2 --> _] Branch to target if less than, short form.
val blt's : target : 'x Label -> Op<'x S S, 'x>
/// [_, value1, value2 --> _] Branch to target if less than (unsigned or unordered).
val blt'un : target : 'x Label -> Op<'x S S, 'x>
/// [_, value1, value2 --> _] Branch to target if less than (unsigned or unordered), short form.
val blt'un's : target : 'x Label -> Op<'x S S, 'x>
/// [_, value1, value2 --> _] Branch to target if unequal or unordered.
val bne'un : target : 'x Label -> Op<'x S S, 'x>
/// [_, value1, value2 --> _] Branch to target if unequal or unordered, short form.
val bne'un's : target : 'x Label -> Op<'x S S, 'x>
/// [_, val --> _, obj] Convert a boxable value of type `valueTy` to its boxed form.
val box'val : valueTy : Type -> Op<'x S, 'x S>
 // [_ --> _] Branch unconditionally to target.
val br : target : 'x Label -> Op<'x, 'y>
/// [_ --> _] Branch unconditionally to target, short form.
val br's : target : 'x Label -> Op<'x, 'y>
/// [_, value --> _] Branch to target if value is zero or null.
val brfalse : target : 'x Label -> Op<'x S, 'x>
/// [_, value --> _] Branch to target if value is zero or null, short form.
val brfalse's : target : 'x Label -> Op<'x S, 'x>
/// [_, value --> _] Branch to target if value is non-zero or non-null.
val brtrue : target : 'x Label -> Op<'x S, 'x>
/// [_, value --> _] Branch to target if value is non-zero or non-null, short form.
val brtrue's : target : 'x Label -> Op<'x S, 'x>
/// [_, obj --> _, obj2] Cast obj to `toTy`.
val castclass : toTy : Type -> Op<'x S, 'x S>
/// [_, value1, value2 --> _, result] Push 1 (of type int32) if value1 equals value2, else push 0.
val ceq : Op<'x S S, 'x S>
/// [_, value1, value2 --> _, result] Push 1 (of type int32) if value1 > value2, else push 0.
val cgt : Op<'x S S, 'x S>
/// [_, value1, value2 --> _, result] Push 1 (of type int32) if value1 > value2, unsigned or unordered.
val cgt'un : Op<'x S S, 'x S>
/// [_, value --> _, value] Throw ArithmeticException if value is not a finite number.
val ckfinite : Op<'x S, 'x>
/// [_, value1, value2 --> _, result] Push 1 (of type int32) if value1 < value2, else push 0.
val clt : Op<'x S S, 'x S>
/// [_, value1, value2 --> _, result] Push 1 (of type int32) if value1 < value2, unsigned or unordered.
val clt'un : Op<'x S S, 'x S>
/// [ ptr, arg1, ... argN --> _, ptr, arg1, ... argN ] Constrain following `callvirt` to call method of `ty`.
val constrained : ty : Type -> Op<'x, 'x>
/// [_, value --> _, result] Convert to native int, pushing native int on stack.
val conv'i : Op<'x S, 'x S>
/// [_, value --> _, result] Convert to int8, pushing int32 on stack.
val conv'i1 : Op<'x S, 'x S>
/// [_, value --> _, result] Convert to int16, pushing int32 on stack.
val conv'i2 : Op<'x S, 'x S>
/// [_, value --> _, result] Convert to int32, pushing int32 on stack.
val conv'i4 : Op<'x S, 'x S>
/// [_, value --> _, result] Convert to int64, pushing int64 on stack.
val conv'i8 : Op<'x S, 'x S>
/// [_, value --> _, result] Convert to native int with overflow check, pushing native int on stack.
val conv'ovf'i : Op<'x S, 'x S>
/// [_, value --> _, result] Convert unsigned to native int with overflow check, pushing native int on stack.
val conv'ovf'i'un : Op<'x S, 'x S>
/// [_, value --> _, result] Convert to int8 with overflow check, pushing int32 on stack.
val conv'ovf'i1 : Op<'x S, 'x S>
/// [_, value --> _, result] Convert unsigned to int8 with overflow check, pushing int32 on stack.
val conv'ovf'i1'un : Op<'x S, 'x S>
/// [_, value --> _, result] Convert to int16 with overflow check, pushing int32 on stack.
val conv'ovf'i2 : Op<'x S, 'x S>
/// [_, value --> _, result] Convert unsigned to int16 with overflow check, pushing int32 on stack.
val conv'ovf'i2'un : Op<'x S, 'x S>
/// [_, value --> _, result] Convert to int16 with overflow check, pushing int32 on stack.
val conv'ovf'i4 : Op<'x S, 'x S>
/// [_, value --> _, result] Convert unsigned to int32 with overflow check, pushing int32 on stack.
val conv'ovf'i4'un : Op<'x S, 'x S>
/// [_, value --> _, result] Convert to int64 with overflow check, pushing int64 on stack.
val conv'ovf'i8 : Op<'x S, 'x S>
/// [_, value --> _, result] Convert unsigned to int64 with overflow check, pushing int64 on stack.
val conv'ovf'i8'un : Op<'x S, 'x S>
/// [_, value --> _, result] Convert to native unsigned int with overflow check, pushing native int on stack.
val conv'ovf'u : Op<'x S, 'x S>
/// [_, value --> _, result] Convert unsigned to native unsigned int with overflow check, pushing native int on stack.
val conv'ovf'u'un : Op<'x S, 'x S>
/// [_, value --> _, result] Convert to uint8 with overflow check, pushing int32 on stack.
val conv'ovf'u1 : Op<'x S, 'x S>
/// [_, value --> _, result] Convert unsigned to uint8 with overflow check, pushing int32 on stack.
val conv'ovf'u1'un : Op<'x S, 'x S>
/// [_, value --> _, result] Convert to uint16 with overflow check, pushing int32 on stack.
val conv'ovf'u2 : Op<'x S, 'x S>
/// [_, value --> _, result] Convert unsigned to uint16 with overflow check, pushing int32 on stack.
val conv'ovf'u2'un : Op<'x S, 'x S>
/// [_, value --> _, result] Convert to uint32 with overflow check, pushing int32 on stack.
val conv'ovf'u4 : Op<'x S, 'x S>
/// [_, value --> _, result] Convert unsigned to uint32 with overflow check, pushing int32 on stack.
val conv'ovf'u4'un : Op<'x S, 'x S>
/// [_, value --> _, result] Convert to uint64 with overflow check, pushing int64 on stack.
val conv'ovf'u8 : Op<'x S, 'x S>
/// [_, value --> _, result] Convert unsigned to uint64 with overflow check, pushing int64 on stack.
val conv'ovf'u8'un : Op<'x S, 'x S>
/// [_, value --> _, result] Convert unsigned to float32, pushing float32 on stack.
val conv'r'un : Op<'x S, 'x S>
/// [_, value --> _, result] Convert to float32, pushing float32 on stack.
val conv'r4 : Op<'x S, 'x S>
/// [_, value --> _, result] Convert to float64, pushing float64 on stack.
val conv'r8 : Op<'x S, 'x S>
/// [_, value --> _, result] Convert to native unsigned int, pushing native int on stack.
val conv'u : Op<'x S, 'x S>
/// [_, value --> _, result] Convert to uint8, pushing int32 on stack.
val conv'u1 : Op<'x S, 'x S>
/// [_, value --> _, result] Convert to uint16, pushing int32 on stack.
val conv'u2 : Op<'x S, 'x S>
/// [_, value --> _, result] Convert to uint32, pushing int32 on stack.
val conv'u4 : Op<'x S, 'x S>
/// [_, value --> _, result] Convert to uint64, pushing int64 on stack.
val conv'u8 : Op<'x S, 'x S>
/// [_, destaddr, srcaddr, size --> _] Copy data from memory to memory.
val cpblk : Op<'x S S S, 'x>
/// [_, dest, src --> _,] Copy a value type from src to dest.
val cpobj : ty : Type -> Op<'x S S, 'x>
/// [_, value1, value2 --> _, result] Divide two values to return a quotient or floating-point result.
val div : Op<'x S S, 'x S>
/// [_, value1, value2 --> _, result] Divide two values, unsigned, returning a quotient.
val div'un : Op<'x S S, 'x S>
/// [_, value --> _, value, value] Duplicate the value on the top of the stack.
val dup : Op<'x S, 'x S S>
/// [_, value --> _] End an exception handling filter clause.
val endfilter : Op<'x S, 'x>
/// [_ --> _] End finally clause of an exception block.
val endfinally : Op<'x, 'x>
/// [_, addr, value, size --> _] Set all bytes in a block of memory to a given byte value.
val initblk : Op<'x S S S, 'x>
/// [_, dest --> _,] Initialize the value at address dest.
val initobj : ty : Type -> Op<'x S, 'x>
/// [_, obj --> _, result] Test if obj is an instance of `ty`, returning null or an instance of `ty`.
val isinst : ty : Type -> Op<'x S, 'x S>
/// [_ --> _] Exit current method and jump to the specified method.
val jmp : MethodInfo -> Op<E, E>
/// [_ --> _, arg] Load argument numbered num onto the stack.
val ldarg : num : int -> Op<'x, 'x S>
/// [_ --> _, addr] Load address of argument numbered num onto the stack.
val ldarga : num : int -> Op<'x, 'x S>
/// [_ --> _, value] Load int32 value onto the stack.
val ldc'i4 : int -> Op<'x, 'x S>
/// [_ --> _, value] Load int64 value onto the stack.
val ldc'i8 : int64 -> Op<'x, 'x S>
/// [_ --> _, value] Load float32 value onto the stack.
val ldc'r4 : single -> Op<'x, 'x S>
/// [_ --> _, value] Load float64 value onto the stack.
val ldc'r8 : double -> Op<'x, 'x S>
/// [_, array, index --> _, value] Load the element at index onto the top of the stack.
val ldelem : elemTy : Type -> Op<'x S S, 'x S>
/// [_, array, index --> _, value] Load the native integer at index onto the top of the stack.
val ldelem'i : Op<'x S S, 'x S>
/// [_, array, index --> _, value] Load the int8 at index onto the top of the stack.
val ldelem'i1 : Op<'x S S, 'x S>
/// [_, array, index --> _, value] Load the int16 at index onto the top of the stack.
val ldelem'i2 : Op<'x S S, 'x S>
/// [_, array, index --> _, value] Load the int32 at index onto the top of the stack.
val ldelem'i4 : Op<'x S S, 'x S>
/// [_, array, index --> _, value] Load the int64 at index onto the top of the stack.
val ldelem'i8 : Op<'x S S, 'x S>
/// [_, array, index --> _, value] Load the float32 at index onto the top of the stack.
val ldelem'r4 : Op<'x S S, 'x S>
/// [_, array, index --> _, value] Load the float64 at index onto the top of the stack.
val ldelem'r8 : Op<'x S S, 'x S>
/// [_, array, index --> _, value] Load the object reference at index onto the top of the stack.
val ldelem'ref : Op<'x S S, 'x S>
/// [_, array, index --> _, value] Load the uint8 at index onto the top of the stack.
val ldelem'u1 : Op<'x S S, 'x S>
/// [_, array, index --> _, value] Load the uint16 at index onto the top of the stack.
val ldelem'u2 : Op<'x S S, 'x S>
/// [_, array, index --> _, value] Load the uint32 at index onto the top of the stack.
val ldelem'u4 : Op<'x S S, 'x S>
/// [_, array, index --> _, address] Load the address of element at index onto the top of the stack.
val ldelema : ty : Type -> Op<'x S S, 'x S>
/// [_, obj --> _, value] Push the value of `field` of object (or value type) obj, onto the stack.
val ldfld : field : FieldInfo -> Op<'x S, 'x S>
/// [_, obj --> _, address] Push the address of `field` of object obj on the stack.
val ldflda : field : FieldInfo -> Op<'x S, 'x S>
/// [_ --> _, ftn] Push a pointer to a method referenced by method `meth` on the stack.
val ldftn : meth : MethodInfo -> Op<'x, 'x S>
/// [_, addr --> _, value] Indirect load value of type native int as native int on the stack.
val ldind'i : Op<'x S, 'x S>
/// [_, addr --> _, value] Indirect load value of type int8 as int32 on the stack.
val ldind'i1 : Op<'x S, 'x S>
/// [_, addr --> _, value] Indirect load value of type int16 as int32 on the stack.
val ldind'i2 : Op<'x S, 'x S>
/// [_, addr --> _, value] Indirect load value of type int32 as int32 on the stack.
val ldind'i4 : Op<'x S, 'x S>
/// [_, addr --> _, value] Indirect load value of type int64 as int64 on the stack.
val ldind'i8 : Op<'x S, 'x S>
/// [_, addr --> _, value] Indirect load value of type float32 as F on the stack.
val ldind'r4 : Op<'x S, 'x S>
/// [_, addr --> _, value] Indirect load value of type float64 as F on the stack.
val ldind'r8 : Op<'x S, 'x S>
/// [_, addr --> _, value] Indirect load value of type object ref as O on the stack.
val ldind'ref : Op<'x S, 'x S>
/// [_, addr --> _, value] Indirect load value of type uint8 as int32 on the stack.
val ldind'u1 : Op<'x S, 'x S>
/// [_, addr --> _, value] Indirect load value of type uint16 as int32 on the stack.
val ldind'u2 : Op<'x S, 'x S>
/// [_, addr --> _, value] Indirect load value of type uint32 as int32 on the stack.
val ldind'u4 : Op<'x S, 'x S>
/// [_, array --> _, length] Push the length (of type native unsigned int) of array on the stack.
val ldlen : Op<'x S, 'x S>
/// [_ --> _, value] Load the value of `local` on the stack.
val ldloc : local : Local -> Op<'x, 'x S>
/// [_ --> _, value] Load the address of `local` on the stack.
val ldloca : local : Local -> Op<'x, 'x S>
/// [_ --> _, null value] Push a null reference on the stack.
val ldnull : Op<'x, 'x S>
/// [_, src --> _, val] Copy the value stored at address src to the stack.
val ldobj : ty : Type -> Op<'x S, 'x S>
/// [_, --> _, value] Push the value of `field` on the stack.
val ldsfld : field: FieldInfo -> Op<'x, 'x S>
/// [_, --> _, value] Push the address of `field` on the stack.
val ldsflda : field : FieldInfo -> Op<'x, 'x S>
/// [_, --> _, str] Push a literal string on the stack.
val ldstr : str : string -> Op<'x, 'x S>
/// [_ --> _, RuntimeHandle] Push runtime handle to a type on the stack.
val ldtoken : ty : Type -> Op<'x, 'x S>
/// [_, object --> _, ftn] Push address of virtual method `meth` on the stack.
val ldvirtftn : meth : MethodInfo -> Op<'x S, 'x S>
/// [--> _] Exit a protected region of code.
val leave : E Label -> Op<'y, 'z>
/// [--> _] Exit a protected region of code, short form.
val leave's : E Label -> Op<'y, 'z>
/// [size --> address] Allocate space from the local memory pool.
val localloc : Op<'x S, 'x S>
/// [_, ptr --> _, typedRef] Push a typed reference to ptr of type `ty` onto the stack.
val mkrefany : ty : Type -> Op<'x S, 'x S>
/// [_, value1, value2 --> _, result] Multiply values.
val mul : Op<'x S S, 'x S>
/// [_, value1, value2 --> _, result] Multiply signed integer values with overflow check.
val mul'ovf : Op<'x S S, 'x S>
/// [_, value1, value2 --> _, result] Multiply unsigned integer values with overflow check.
val mul'ovf'un : Op<'x S S, 'x S>
/// [_, value --> _, result] Negate value.
val neg : Op<'x S, 'x S>
/// [_, numElems --> _, array] Create a new array with elements of type `elemTy`.
val newarr : elemTy : Type -> Op<'x S, 'x S>
/// [_, --> _,] No-op.
val nop : Op<'x, 'x>
/// [_, value --> _] Pop value from the stack.
val pop : Op<'x S, 'x>
/// [_, TypedRef --> _, type] Push the type token stored in a typed reference.
val refanytype : Op<'x S, 'x S>
/// [_, TypedRef --> _, address] Push the address stored in a typed reference.
val refanyval : ty : Type -> Op<'x S, 'x S>
/// [_, value1, value2 --> _, result] Remainder when dividing one value by another.
val rem : Op<'x S S, 'x S>
/// [_, value1, value2 --> _, result] Remainder when dividing one unsigned value by another.
val rem'un : Op<'x S S, 'x S>
/// [ retVal --> ] Return from method with a value.
val ret : Op<E S, E>
/// [ --> ] Return from method without a value.
val ret'void : Op<E, E>
/// [_ --> _] Rethrow the current exception.
val rethrow : Op<'x, 'x>
/// [_, value, shiftAmount --> _, result] Shift an integer left (shifting in zeros), return an integer.
val shl : Op<'x S S, 'x S>
/// [_, value, shiftAmount --> _, result] Shift an integer right (shift in sign), return an integer.
val shr : Op<'x S S, 'x S>
/// [_, value, shiftAmount --> _, result] Shift an integer right (shift in zero), return an integer.
val shr'un : Op<'x S S, 'x S>
/// [_, --> _, size] Push the size, in bytes, of `valueTy` as an unsigned int32.
val sizeof : valueTy : Type -> Op<'x, 'x S>
/// [_, value --> _,] Store value to the argument numbered num.
val starg : num : int -> Op<'x S, 'x>
/// [_, array, index, value, --> _] Replace array element at index with the value on the stack.
val stelem : elemTy : Type -> Op<'x S S S, 'x>
/// [_, array, index, value, --> _] Replace array element at index with the native int on the stack.
val stelem'i : Op<'x S S S, 'x>
/// [_, array, index, value, --> _] Replace array element at index with the int8 on the stack.
val stelem'i1 : Op<'x S S S, 'x>
/// [_, array, index, value, --> _] Replace array element at index with the int16 on the stack.
val stelem'i2 : Op<'x S S S, 'x>
/// [_, array, index, value, --> _] Replace array element at index with the int32 on the stack.
val stelem'i4 : Op<'x S S S, 'x>
/// [_, array, index, value, --> _] Replace array element at index with the int64 on the stack.
val stelem'i8 : Op<'x S S S, 'x>
/// [_, array, index, value, --> _] Replace array element at index with the float32 on the stack.
val stelem'r4 : Op<'x S S S, 'x>
/// [_, array, index, value, --> _] Replace array element at index with the float64 on the stack.
val stelem'r8 : Op<'x S S S, 'x>
/// [_, array, index, value, --> _] Replace array element at index with the object reference on the stack.
val stelem'ref : Op<'x S S S, 'x>
/// [_, obj, value --> _,] Replace the value of `field` of the object obj with value.
val stfld : field : FieldInfo -> Op<'x S S, 'x>
/// [_, addr, val --> _] Store value of type native int into memory at address.
val stind'i : Op<'x S S, 'x>
/// [_, addr, val --> _] Store value of type int8 into memory at address.
val stind'i1 : Op<'x S S, 'x>
/// [_, addr, val --> _] Store value of type int16 into memory at address.
val stind'i2 : Op<'x S S, 'x>
/// [_, addr, val --> _] Store value of type int32 into memory at address.
val stind'i4 : Op<'x S S, 'x>
/// [_, addr, val --> _] Store value of type int64 into memory at address.
val stind'i8 : Op<'x S S, 'x>
/// [_, addr, val --> _] Store value of type float32 into memory at address.
val stind'r4 : Op<'x S S, 'x>
/// [_, addr, val --> _] Store value of type float64 into memory at address.
val stind'r8 : Op<'x S S, 'x>
/// [_, addr, val --> _] Store value of type object reference into memory at address.
val stind'ref : Op<'x S S, 'x>
/// [_, value --> _] Pop a value from stack into local variable `local`.
val stloc : local : Local -> Op<'x S, 'x>
/// [_, dest, src --> _,] Store a value of type `ty` at an address.
val stobj : ty : Type -> Op<'x S S, 'x>
/// [_, val --> _,] Replace the value of `field` with val.
val stsfld : field : FieldInfo -> Op<'x S, 'x>
/// [_, value1, value2 --> _, result] Subtract value2 from value1, returning a new value.
val sub : Op<'x S S, 'x S>
/// [_, value1, value2 --> _, result] Subtract native value2 from value1 with overflow check, returning a new value.
val sub'ovf : Op<'x S S, 'x S>
/// [_, value1, value2 --> _, result] Subtract unsigned native value2 from value1 with overflow check, returning a new value.
val sub'ovf'un : Op<'x S S, 'x S>
/// [_, n --> _] Jump to nth label in table, or fall through to next instruction if n is out of range.
val switch : 'x Label seq -> Op<'x S, 'x>
/// [_, _] Mark following call as a tail call.
val tail : Op<'x, 'x>
/// [_, exn --> _] Throw an exception.
val throw : Op<'x S, 'y>
/// [_, addr --> _, addr] Subsequent pointer instruction might be unaligned.
val unaligned : Op<'x S, 'x S>
/// [_, obj --> _, valueTypePtr] Extract a value-type from obj, its boxed representation.
val unbox'val : valueTy : Type -> Op<'x S, 'x S>
/// [_, obj --> _, value or obj] Extract a value-type from obj, its boxed representation
val unbox'any : ty : Type -> Op<'x S, 'x S>
/// [_, addr --> _, addr] Subsequent pointer reference is volatile.
val volatile' : Op<'x S, 'x S>

