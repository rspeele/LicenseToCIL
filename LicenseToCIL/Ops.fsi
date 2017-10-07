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

type CILHelpers =
    class
        static member LdLoc : LocalBuilder * IL -> unit
        static member LdLoca : LocalBuilder * IL -> unit
        static member StLoc : LocalBuilder * IL -> unit
        static member LdcI4 : int * IL -> unit
        static member LdArg : int * IL -> unit
        static member LdArgA : int * IL -> unit
        static member StArg : int * IL -> unit
    end

/// Sequence together two stack operations.
val inline combine : Op<'i, 'm> -> (unit -> Op<'m, 'e>) -> Op<'i, 'e>
/// Emit no instructions and retain stack state.
val inline zero : Op<'x, 'x>
/// Force the stack state to appear as desired to the type checker.
val inline pretend : Op<'x, 'y>

/// Define a new local, e.g. `let! loc = deflocal typeof<int>`.
val deflocal : Type -> LocalDefinition
/// Obtain a temporary local, e.g. `let! loc = tmplocal typeof<int>`.
/// The local may contain an arbitrary previous value.
val tmplocal : Type -> LocalTemporary
/// Define a new label, e.g. `let! lbl = deflabel`.
val deflabel : LabelDefinition

/// Mark the position of a label.
val inline mark : 'x Label -> Op<'x, 'x>

/// [? -> ?] Call method taking an unknown number of arguments.
/// Not recommended -- favor exact variants.
val inline call'x : MethodInfo -> Op<'x, 'y>
/// [_ -> _, value] Call method taking zero arguments and returning a value.
val inline call0 : MethodInfo -> Op<'x, 'x S> 
/// [_, arg0 -> _, value] Call method taking 1 argument and returning a value.
val inline call1 : MethodInfo -> Op<'x S, 'x S>
/// [_, arg0, arg1 -> _, value] Call method taking 2 arguments and returning a value.
val inline call2 : MethodInfo -> Op<'x S S, 'x S>
/// [_, arg0, arg1, arg2 -> _, value] Call method taking 3 arguments and returning a value.
val inline call3 : MethodInfo -> Op<'x S S S, 'x S>
/// [_, arg0, arg1. arg2, arg3 -> _, value] Call method taking 4 arguments and returning a value.
val inline call4 : MethodInfo -> Op<'x S S S S, 'x S>
/// [_, arg0, arg1, arg2, arg3, arg4 -> _, value] Call method taking 5 arguments and returning a value.
val inline call5 : MethodInfo -> Op<'x S S S S S, 'x S>
/// [_, arg0, arg1, arg2, arg3, arg4, arg5 -> _, value] Call method taking 6 arguments and returning a value.
val inline call6 : MethodInfo -> Op<'x S S S S S S, 'x S>

/// [_ -> _] Call void method taking zero arguments.
val inline call0'void : MethodInfo -> Op<'x, 'x> 
/// [_, arg0 -> _] Call void method taking 1 argument.
val inline call1'void : MethodInfo -> Op<'x S, 'x>
/// [_, arg0, arg1 -> _] Call void method taking 2 arguments.
val inline call2'void : MethodInfo -> Op<'x S S, 'x>
/// [_, arg0, arg1, arg2 -> _] Call void method taking 3 arguments.
val inline call3'void : MethodInfo -> Op<'x S S S, 'x>
/// [_, arg0, arg1, arg2, arg3 -> _] Call void method taking 4 arguments.
val inline call4'void : MethodInfo -> Op<'x S S S S, 'x>
/// [_, arg0, arg1, arg2, arg3, arg4 -> _] Call void method taking 5 arguments.
val inline call5'void : MethodInfo -> Op<'x S S S S S, 'x>
/// [_, arg0, arg1, arg2, arg3, arg4, arg5 -> _] Call void method taking 6 arguments.
val inline call6'void : MethodInfo -> Op<'x S S S S S S, 'x>

/// [?, ptr -> ?] Indirectly call method taking an unknown number of arguments.
/// Not recommended -- favor exact variants.
val inline calli'x : Op<'x S, 'y>
/// [_, ptr -> _, value] Indirectly call method taking zero arguments and returning a value.
val inline calli0 : Op<'x S, 'x S>
/// [_, arg0, ptr -> _, value] Indirectly call method taking 1 argument and returning a value.
val inline calli1 : Op<'x S S, 'x S>
/// [_, arg0, arg1, ptr -> _, value] Indirectly call method taking 2 arguments and returning a value.
val inline calli2 : Op<'x S S S, 'x S>
/// [_, arg0, arg1, arg2, ptr -> _, value] Indirectly call method taking 3 arguments and returning a value.
val inline calli3 : Op<'x S S S S, 'x S>
/// [_, arg0, arg1, arg2, arg3, ptr -> _, value] Indirectly call method taking 4 arguments and returning a value.
val inline calli4 : Op<'x S S S S S, 'x S>
/// [_, arg0, arg1, arg2, arg3, arg4, ptr -> _, value] Indirectly call method taking 5 arguments and returning a value.
val inline calli5 : Op<'x S S S S S S, 'x S>

/// [_, ptr -> _] Indirectly call void method taking zero arguments.
val inline calli0'void : Op<'x S, 'x>
/// [_, arg0, ptr -> _] Indirectly call void method taking 1 argument.
val inline calli1'void : Op<'x S S, 'x>
/// [_, arg0, arg1, ptr -> _] Indirectly call void method taking 2 arguments.
val inline calli2'void : Op<'x S S S, 'x>
/// [_, arg0, arg1, arg2, ptr -> _] Indirectly call void method taking 3 arguments.
val inline calli3'void : Op<'x S S S S, 'x>
/// [_, arg0, arg1, arg2, arg3, ptr -> _] Indirectly call void method taking 4 arguments.
val inline calli4'void : Op<'x S S S S S, 'x>
/// [_, arg0, arg1, arg2, arg3, arg4, ptr -> _] Indirectly call void method taking 5 arguments.
val inline calli5'void : Op<'x S S S S S S, 'x>

/// [? -> ?] Indirectly call virtual method taking an unknown number of arguments.
/// Not recommended -- favor exact variants.
val inline callvirt'x : MethodInfo -> Op<'x, 'y>
/// [_, obj -> _, value] Call virtual method taking 1 (instance) argument and returning a value.
val inline callvirt1 : MethodInfo -> Op<'x S, 'x S>
/// [_, obj, arg0 -> _, value] Call virtual method taking 2 arguments and returning a value.
val inline callvirt2 : MethodInfo -> Op<'x S S, 'x S>
/// [_, obj, arg0, arg1 -> _, value] Call virtual method taking 3 arguments and returning a value.
val inline callvirt3 : MethodInfo -> Op<'x S S S, 'x S>
/// [_, obj, arg0, arg1, arg2 -> _, value] Call virtual method taking 4 arguments and returning a value.
val inline callvirt4 : MethodInfo -> Op<'x S S S S, 'x S>
/// [_, obj, arg0, arg1, arg2, arg3 -> _, value] Call virtual method taking 5 arguments and returning a value.
val inline callvirt5 : MethodInfo -> Op<'x S S S S S, 'x S>

/// [_, obj -> _] Call virtual void method taking 1 (instance) argument.
val inline callvirt1'void : MethodInfo -> Op<'x S, 'x>
/// [_, obj, arg0 -> _] Call virtual void method taking 2 arguments.
val inline callvirt2'void : MethodInfo -> Op<'x S S, 'x>
/// [_, obj, arg0, arg1 -> _] Call virtual void method taking 3 arguments.
val inline callvirt3'void : MethodInfo -> Op<'x S S S, 'x>
/// [_, obj, arg0, arg1, arg2 -> _] Call virtual void method taking 4 arguments.
val inline callvirt4'void : MethodInfo -> Op<'x S S S S, 'x>
/// [_, obj, arg0, arg1, arg2, arg3 -> _] Call virtual void method taking 5 arguments.
val inline callvirt5'void : MethodInfo -> Op<'x S S S S S, 'x>

/// [? -> ?] Call constructor taking an unknown number of arguments.
/// Not recommended -- favor exact variants.
val inline newobj'x : ConstructorInfo -> Op<'x, 'y S>
/// [_ -> _, object] Call constructor taking zero arguments.
val inline newobj0 : ConstructorInfo -> Op<'x, 'x S>
/// [_, arg0 -> _, object] Call constructor taking 1 argument.
val inline newobj1 : ConstructorInfo -> Op<'x S, 'x S>
/// [_, arg0, arg1 -> _, object] Call constructor taking 2 arguments.
val inline newobj2 : ConstructorInfo -> Op<'x S S, 'x S>
/// [_, arg0, arg1, arg2 -> _, object] Call constructor taking 3 arguments.
val inline newobj3 : ConstructorInfo -> Op<'x S S S, 'x S>
/// [_, arg0, arg1, arg2, arg3 -> _, object] Call constructor taking 4 arguments.
val inline newobj4 : ConstructorInfo -> Op<'x S S S S, 'x S>
/// [_, arg0, arg1, arg2, arg3, arg4 -> _, object] Call constructor taking 5 arguments.
val inline newobj5 : ConstructorInfo -> Op<'x S S S S S, 'x S>

/// [_, value1, value2 --> _, result] Add two values, returning a new value.
val inline add : Op<'x S S, 'x S>
/// [_, value1, value2 --> _, result] Add signed integer values with overflow check.
val inline add'ovf : Op<'x S S, 'x S>
/// [_, value1, value2 --> _, result] Add unsigned integer values with overflow check.
val inline add'ovf'un : Op<'x S S, 'x S>
/// [_ --> _, argListHandle] Return argument list handle for the current method.
val inline arglist : Op<'x, 'x S>
/// [_, value1, value2 --> _] Branch to target if equal.
val inline beq : target : 'x Label -> Op<'x S S, 'x>
/// [_, value1, value2 --> _] Branch to target if equal, short form.
val inline beq's : target : 'x Label -> Op<'x S S, 'x>
/// [_, value1, value2 --> _] Branch to target if greater than or equal to.
val inline bge : target : 'x Label -> Op<'x S S, 'x>
/// [_, value1, value2 --> _] Branch to target if greater than or equal to, short form.
val inline bge's : target : 'x Label -> Op<'x S S, 'x>
/// [_, value1, value2 --> _] Branch to target if greater than or equal to (unsigned or unordered).
val inline bge'un : target : 'x Label -> Op<'x S S, 'x>
/// [_, value1, value2 --> _] Branch to target if greater than or equal to (unsigned or unordered), short form.
val inline bge'un's : target : 'x Label -> Op<'x S S, 'x>
/// [_, value1, value2 --> _] Branch to target if greater than.
val inline bgt : target : 'x Label -> Op<'x S S, 'x>
/// [_, value1, value2 --> _] Branch to target if greater than, short form.
val inline bgt's : target : 'x Label -> Op<'x S S, 'x>
/// [_, value1, value2 --> _] Branch to target if greater than (unsigned or unordered).
val inline bgt'un : target : 'x Label -> Op<'x S S, 'x>
/// [_, value1, value2 --> _] Branch to target if greater than (unsigned or unordered), short form.
val inline bgt'un's : target : 'x Label -> Op<'x S S, 'x>
/// [_, value1, value2 --> _, result] Bitwise AND of two integer values, returns an integer.
val inline bit'and : Op<'x S S, 'x S>
/// [_, value --> _, result] Bitwise complement of an integer value.
val inline bit'not : Op<'x S, 'x S>
/// [_, value1, value2 --> _, result] Bitwise OR of two integer values, returns an integer.
val inline bit'or : Op<'x S S, 'x S>
/// [_, value1, value2 --> _, result] Bitwise XOR of integer values, returns an integer.
val inline bit'xor : Op<'x S S, 'x S>
/// [_, value1, value2 --> _] Branch to target if less than or equal to.
val inline ble : target : 'x Label -> Op<'x S S, 'x>
/// [_, value1, value2 --> _] Branch to target if less than or equal to, short form.
val inline ble's : target : 'x Label -> Op<'x S S, 'x>
/// [_, value1, value2 --> _] Branch to target if less than or equal to (unsigned or unordered).
val inline ble'un : target : 'x Label -> Op<'x S S, 'x>
/// [_, value1, value2 --> _] Branch to target if less than or equal to (unsigned or unordered), short form.
val inline ble'un's : target : 'x Label -> Op<'x S S, 'x>
/// [_, value1, value2 --> _] Branch to target if less than.
val inline blt : target : 'x Label -> Op<'x S S, 'x>
/// [_, value1, value2 --> _] Branch to target if less than, short form.
val inline blt's : target : 'x Label -> Op<'x S S, 'x>
/// [_, value1, value2 --> _] Branch to target if less than (unsigned or unordered).
val inline blt'un : target : 'x Label -> Op<'x S S, 'x>
/// [_, value1, value2 --> _] Branch to target if less than (unsigned or unordered), short form.
val inline blt'un's : target : 'x Label -> Op<'x S S, 'x>
/// [_, value1, value2 --> _] Branch to target if unequal or unordered.
val inline bne'un : target : 'x Label -> Op<'x S S, 'x>
/// [_, value1, value2 --> _] Branch to target if unequal or unordered, short form.
val inline bne'un's : target : 'x Label -> Op<'x S S, 'x>
/// [_, val --> _, obj] Convert a boxable value of type `valueTy` to its boxed form.
val inline box'val : valueTy : Type -> Op<'x S, 'x S>
 // [_ --> _] Branch unconditionally to target.
val inline br : target : 'x Label -> Op<'x, 'y>
/// [_ --> _] Branch unconditionally to target, short form.
val inline br's : target : 'x Label -> Op<'x, 'y>
/// [_, value --> _] Branch to target if value is zero or null.
val inline brfalse : target : 'x Label -> Op<'x S, 'x>
/// [_, value --> _] Branch to target if value is zero or null, short form.
val inline brfalse's : target : 'x Label -> Op<'x S, 'x>
/// [_, value --> _] Branch to target if value is non-zero or non-null.
val inline brtrue : target : 'x Label -> Op<'x S, 'x>
/// [_, value --> _] Branch to target if value is non-zero or non-null, short form.
val inline brtrue's : target : 'x Label -> Op<'x S, 'x>
/// [_, obj --> _, obj2] Cast obj to `toTy`.
val inline castclass : toTy : Type -> Op<'x S, 'x S>
/// [_, value1, value2 --> _, result] Push 1 (of type int32) if value1 equals value2, else push 0.
val inline ceq : Op<'x S S, 'x S>
/// [_, value1, value2 --> _, result] Push 1 (of type int32) if value1 > value2, else push 0.
val inline cgt : Op<'x S S, 'x S>
/// [_, value1, value2 --> _, result] Push 1 (of type int32) if value1 > value2, unsigned or unordered.
val inline cgt'un : Op<'x S S, 'x S>
/// [_, value --> _, value] Throw ArithmeticException if value is not a finite number.
val inline ckfinite : Op<'x S, 'x>
/// [_, value1, value2 --> _, result] Push 1 (of type int32) if value1 < value2, else push 0.
val inline clt : Op<'x S S, 'x S>
/// [_, value1, value2 --> _, result] Push 1 (of type int32) if value1 < value2, unsigned or unordered.
val inline clt'un : Op<'x S S, 'x S>
/// [ ptr, arg1, ... argN --> _, ptr, arg1, ... argN ] Constrain following `callvirt` to call method of `ty`.
val inline constrained : ty : Type -> Op<'x, 'x>
/// [_, value --> _, result] Convert to native int, pushing native int on stack.
val inline conv'i : Op<'x S, 'x S>
/// [_, value --> _, result] Convert to int8, pushing int32 on stack.
val inline conv'i1 : Op<'x S, 'x S>
/// [_, value --> _, result] Convert to int16, pushing int32 on stack.
val inline conv'i2 : Op<'x S, 'x S>
/// [_, value --> _, result] Convert to int32, pushing int32 on stack.
val inline conv'i4 : Op<'x S, 'x S>
/// [_, value --> _, result] Convert to int64, pushing int64 on stack.
val inline conv'i8 : Op<'x S, 'x S>
/// [_, value --> _, result] Convert to native int with overflow check, pushing native int on stack.
val inline conv'ovf'i : Op<'x S, 'x S>
/// [_, value --> _, result] Convert unsigned to native int with overflow check, pushing native int on stack.
val inline conv'ovf'i'un : Op<'x S, 'x S>
/// [_, value --> _, result] Convert to int8 with overflow check, pushing int32 on stack.
val inline conv'ovf'i1 : Op<'x S, 'x S>
/// [_, value --> _, result] Convert unsigned to int8 with overflow check, pushing int32 on stack.
val inline conv'ovf'i1'un : Op<'x S, 'x S>
/// [_, value --> _, result] Convert to int16 with overflow check, pushing int32 on stack.
val inline conv'ovf'i2 : Op<'x S, 'x S>
/// [_, value --> _, result] Convert unsigned to int16 with overflow check, pushing int32 on stack.
val inline conv'ovf'i2'un : Op<'x S, 'x S>
/// [_, value --> _, result] Convert to int16 with overflow check, pushing int32 on stack.
val inline conv'ovf'i4 : Op<'x S, 'x S>
/// [_, value --> _, result] Convert unsigned to int32 with overflow check, pushing int32 on stack.
val inline conv'ovf'i4'un : Op<'x S, 'x S>
/// [_, value --> _, result] Convert to int64 with overflow check, pushing int64 on stack.
val inline conv'ovf'i8 : Op<'x S, 'x S>
/// [_, value --> _, result] Convert unsigned to int64 with overflow check, pushing int64 on stack.
val inline conv'ovf'i8'un : Op<'x S, 'x S>
/// [_, value --> _, result] Convert to native unsigned int with overflow check, pushing native int on stack.
val inline conv'ovf'u : Op<'x S, 'x S>
/// [_, value --> _, result] Convert unsigned to native unsigned int with overflow check, pushing native int on stack.
val inline conv'ovf'u'un : Op<'x S, 'x S>
/// [_, value --> _, result] Convert to uint8 with overflow check, pushing int32 on stack.
val inline conv'ovf'u1 : Op<'x S, 'x S>
/// [_, value --> _, result] Convert unsigned to uint8 with overflow check, pushing int32 on stack.
val inline conv'ovf'u1'un : Op<'x S, 'x S>
/// [_, value --> _, result] Convert to uint16 with overflow check, pushing int32 on stack.
val inline conv'ovf'u2 : Op<'x S, 'x S>
/// [_, value --> _, result] Convert unsigned to uint16 with overflow check, pushing int32 on stack.
val inline conv'ovf'u2'un : Op<'x S, 'x S>
/// [_, value --> _, result] Convert to uint32 with overflow check, pushing int32 on stack.
val inline conv'ovf'u4 : Op<'x S, 'x S>
/// [_, value --> _, result] Convert unsigned to uint32 with overflow check, pushing int32 on stack.
val inline conv'ovf'u4'un : Op<'x S, 'x S>
/// [_, value --> _, result] Convert to uint64 with overflow check, pushing int64 on stack.
val inline conv'ovf'u8 : Op<'x S, 'x S>
/// [_, value --> _, result] Convert unsigned to uint64 with overflow check, pushing int64 on stack.
val inline conv'ovf'u8'un : Op<'x S, 'x S>
/// [_, value --> _, result] Convert unsigned to float32, pushing float32 on stack.
val inline conv'r'un : Op<'x S, 'x S>
/// [_, value --> _, result] Convert to float32, pushing float32 on stack.
val inline conv'r4 : Op<'x S, 'x S>
/// [_, value --> _, result] Convert to float64, pushing float64 on stack.
val inline conv'r8 : Op<'x S, 'x S>
/// [_, value --> _, result] Convert to native unsigned int, pushing native int on stack.
val inline conv'u : Op<'x S, 'x S>
/// [_, value --> _, result] Convert to uint8, pushing int32 on stack.
val inline conv'u1 : Op<'x S, 'x S>
/// [_, value --> _, result] Convert to uint16, pushing int32 on stack.
val inline conv'u2 : Op<'x S, 'x S>
/// [_, value --> _, result] Convert to uint32, pushing int32 on stack.
val inline conv'u4 : Op<'x S, 'x S>
/// [_, value --> _, result] Convert to uint64, pushing int64 on stack.
val inline conv'u8 : Op<'x S, 'x S>
/// [_, destaddr, srcaddr, size --> _] Copy data from memory to memory.
val inline cpblk : Op<'x S S S, 'x>
/// [_, dest, src --> _,] Copy a value type from src to dest.
val inline cpobj : ty : Type -> Op<'x S S, 'x>
/// [_, value1, value2 --> _, result] Divide two values to return a quotient or floating-point result.
val inline div : Op<'x S S, 'x S>
/// [_, value1, value2 --> _, result] Divide two values, unsigned, returning a quotient.
val inline div'un : Op<'x S S, 'x S>
/// [_, value --> _, value, value] Duplicate the value on the top of the stack.
val inline dup : Op<'x S, 'x S S>
/// [_, value --> _] End an exception handling filter clause.
val inline endfilter : Op<'x S, 'x>
/// [_ --> _] End finally clause of an exception block.
val inline endfinally : Op<'x, 'x>
/// [_, addr, value, size --> _] Set all bytes in a block of memory to a given byte value.
val inline initblk : Op<'x S S S, 'x>
/// [_, dest --> _,] Initialize the value at address dest.
val inline initobj : ty : Type -> Op<'x S, 'x>
/// [_, obj --> _, result] Test if obj is an instance of `ty`, returning null or an instance of `ty`.
val inline isinst : ty : Type -> Op<'x S, 'x S>
/// [_ --> _] Exit current method and jump to the specified method.
val inline jmp : MethodInfo -> Op<E, E>
/// [_ --> _, arg] Load argument numbered num onto the stack.
val inline ldarg : num : int -> Op<'x, 'x S>
/// [_ --> _, addr] Load address of argument numbered num onto the stack.
val inline ldarga : num : int -> Op<'x, 'x S>
/// [_ --> _, value] Load int32 value onto the stack.
val inline ldc'i4 : int -> Op<'x, 'x S>
/// [_ --> _, value] Load int64 value onto the stack.
val inline ldc'i8 : int64 -> Op<'x, 'x S>
/// [_ --> _, value] Load float32 value onto the stack.
val inline ldc'r4 : single -> Op<'x, 'x S>
/// [_ --> _, value] Load float64 value onto the stack.
val inline ldc'r8 : double -> Op<'x, 'x S>
/// [_, array, index --> _, value] Load the element at index onto the top of the stack.
val inline ldelem : elemTy : Type -> Op<'x S S, 'x S>
/// [_, array, index --> _, value] Load the native integer at index onto the top of the stack.
val inline ldelem'i : Op<'x S S, 'x S>
/// [_, array, index --> _, value] Load the int8 at index onto the top of the stack.
val inline ldelem'i1 : Op<'x S S, 'x S>
/// [_, array, index --> _, value] Load the int16 at index onto the top of the stack.
val inline ldelem'i2 : Op<'x S S, 'x S>
/// [_, array, index --> _, value] Load the int32 at index onto the top of the stack.
val inline ldelem'i4 : Op<'x S S, 'x S>
/// [_, array, index --> _, value] Load the int64 at index onto the top of the stack.
val inline ldelem'i8 : Op<'x S S, 'x S>
/// [_, array, index --> _, value] Load the float32 at index onto the top of the stack.
val inline ldelem'r4 : Op<'x S S, 'x S>
/// [_, array, index --> _, value] Load the float64 at index onto the top of the stack.
val inline ldelem'r8 : Op<'x S S, 'x S>
/// [_, array, index --> _, value] Load the object reference at index onto the top of the stack.
val inline ldelem'ref : Op<'x S S, 'x S>
/// [_, array, index --> _, value] Load the uint8 at index onto the top of the stack.
val inline ldelem'u1 : Op<'x S S, 'x S>
/// [_, array, index --> _, value] Load the uint16 at index onto the top of the stack.
val inline ldelem'u2 : Op<'x S S, 'x S>
/// [_, array, index --> _, value] Load the uint32 at index onto the top of the stack.
val inline ldelem'u4 : Op<'x S S, 'x S>
/// [_, array, index --> _, address] Load the address of element at index onto the top of the stack.
val inline ldelema : ty : Type -> Op<'x S S, 'x S>
/// [_, obj --> _, value] Push the value of `field` of object (or value type) obj, onto the stack.
val inline ldfld : field : FieldInfo -> Op<'x S, 'x S>
/// [_, obj --> _, address] Push the address of `field` of object obj on the stack.
val inline ldflda : field : FieldInfo -> Op<'x S, 'x S>
/// [_ --> _, ftn] Push a pointer to a method referenced by method `meth` on the stack.
val inline ldftn : meth : MethodInfo -> Op<'x, 'x S>
/// [_, addr --> _, value] Indirect load value of type native int as native int on the stack.
val inline ldind'i : Op<'x S, 'x S>
/// [_, addr --> _, value] Indirect load value of type int8 as int32 on the stack.
val inline ldind'i1 : Op<'x S, 'x S>
/// [_, addr --> _, value] Indirect load value of type int16 as int32 on the stack.
val inline ldind'i2 : Op<'x S, 'x S>
/// [_, addr --> _, value] Indirect load value of type int32 as int32 on the stack.
val inline ldind'i4 : Op<'x S, 'x S>
/// [_, addr --> _, value] Indirect load value of type int64 as int64 on the stack.
val inline ldind'i8 : Op<'x S, 'x S>
/// [_, addr --> _, value] Indirect load value of type float32 as F on the stack.
val inline ldind'r4 : Op<'x S, 'x S>
/// [_, addr --> _, value] Indirect load value of type float64 as F on the stack.
val inline ldind'r8 : Op<'x S, 'x S>
/// [_, addr --> _, value] Indirect load value of type object ref as O on the stack.
val inline ldind'ref : Op<'x S, 'x S>
/// [_, addr --> _, value] Indirect load value of type uint8 as int32 on the stack.
val inline ldind'u1 : Op<'x S, 'x S>
/// [_, addr --> _, value] Indirect load value of type uint16 as int32 on the stack.
val inline ldind'u2 : Op<'x S, 'x S>
/// [_, addr --> _, value] Indirect load value of type uint32 as int32 on the stack.
val inline ldind'u4 : Op<'x S, 'x S>
/// [_, array --> _, length] Push the length (of type native unsigned int) of array on the stack.
val inline ldlen : Op<'x S, 'x S>
/// [_ --> _, value] Load the value of `local` on the stack.
val inline ldloc : local : Local -> Op<'x, 'x S>
/// [_ --> _, value] Load the address of `local` on the stack.
val inline ldloca : local : Local -> Op<'x, 'x S>
/// [_ --> _, null value] Push a null reference on the stack.
val inline ldnull : Op<'x, 'x S>
/// [_, src --> _, val] Copy the value stored at address src to the stack.
val inline ldobj : ty : Type -> Op<'x S, 'x S>
/// [_, --> _, value] Push the value of `field` on the stack.
val inline ldsfld : field: FieldInfo -> Op<'x, 'x S>
/// [_, --> _, value] Push the address of `field` on the stack.
val inline ldsflda : field : FieldInfo -> Op<'x, 'x S>
/// [_, --> _, str] Push a literal string on the stack.
val inline ldstr : str : string -> Op<'x, 'x S>
/// [_ --> _, RuntimeHandle] Push runtime handle to a type on the stack.
val inline ldtoken : ty : Type -> Op<'x, 'x S>
/// [_, object --> _, ftn] Push address of virtual method `meth` on the stack.
val inline ldvirtftn : meth : MethodInfo -> Op<'x S, 'x S>
/// [--> _] Exit a protected region of code.
val inline leave : E Label -> Op<'y, 'z>
/// [--> _] Exit a protected region of code, short form.
val inline leave's : E Label -> Op<'y, 'z>
/// [size --> address] Allocate space from the local memory pool.
val inline localloc : Op<'x S, 'x S>
/// [_, ptr --> _, typedRef] Push a typed reference to ptr of type `ty` onto the stack.
val inline mkrefany : ty : Type -> Op<'x S, 'x S>
/// [_, value1, value2 --> _, result] Multiply values.
val inline mul : Op<'x S S, 'x S>
/// [_, value1, value2 --> _, result] Multiply signed integer values with overflow check.
val inline mul'ovf : Op<'x S S, 'x S>
/// [_, value1, value2 --> _, result] Multiply unsigned integer values with overflow check.
val inline mul'ovf'un : Op<'x S S, 'x S>
/// [_, value --> _, result] Negate value.
val inline neg : Op<'x S, 'x S>
/// [_, numElems --> _, array] Create a new array with elements of type `elemTy`.
val inline newarr : elemTy : Type -> Op<'x S, 'x S>
/// [_, --> _,] No-op.
val inline nop : Op<'x, 'x>
/// [_, value --> _] Pop value from the stack.
val inline pop : Op<'x S, 'x>
/// [_, TypedRef --> _, type] Push the type token stored in a typed reference.
val inline refanytype : Op<'x S, 'x S>
/// [_, TypedRef --> _, address] Push the address stored in a typed reference.
val inline refanyval : ty : Type -> Op<'x S, 'x S>
/// [_, value1, value2 --> _, result] Remainder when dividing one value by another.
val inline rem : Op<'x S S, 'x S>
/// [_, value1, value2 --> _, result] Remainder when dividing one unsigned value by another.
val inline rem'un : Op<'x S S, 'x S>
/// [ retVal --> ] Return from method with a value.
val inline ret : Op<E S, 'x>
/// [ --> ] Return from method without a value.
val inline ret'void : Op<E, 'x>
/// [_ --> _] Rethrow the current exception.
val inline rethrow : Op<'x, 'x>
/// [_, value, shiftAmount --> _, result] Shift an integer left (shifting in zeros), return an integer.
val inline shl : Op<'x S S, 'x S>
/// [_, value, shiftAmount --> _, result] Shift an integer right (shift in sign), return an integer.
val inline shr : Op<'x S S, 'x S>
/// [_, value, shiftAmount --> _, result] Shift an integer right (shift in zero), return an integer.
val inline shr'un : Op<'x S S, 'x S>
/// [_, --> _, size] Push the size, in bytes, of `valueTy` as an unsigned int32.
val inline sizeof : valueTy : Type -> Op<'x, 'x S>
/// [_, value --> _,] Store value to the argument numbered num.
val inline starg : num : int -> Op<'x S, 'x>
/// [_, array, index, value, --> _] Replace array element at index with the value on the stack.
val inline stelem : elemTy : Type -> Op<'x S S S, 'x>
/// [_, array, index, value, --> _] Replace array element at index with the native int on the stack.
val inline stelem'i : Op<'x S S S, 'x>
/// [_, array, index, value, --> _] Replace array element at index with the int8 on the stack.
val inline stelem'i1 : Op<'x S S S, 'x>
/// [_, array, index, value, --> _] Replace array element at index with the int16 on the stack.
val inline stelem'i2 : Op<'x S S S, 'x>
/// [_, array, index, value, --> _] Replace array element at index with the int32 on the stack.
val inline stelem'i4 : Op<'x S S S, 'x>
/// [_, array, index, value, --> _] Replace array element at index with the int64 on the stack.
val inline stelem'i8 : Op<'x S S S, 'x>
/// [_, array, index, value, --> _] Replace array element at index with the float32 on the stack.
val inline stelem'r4 : Op<'x S S S, 'x>
/// [_, array, index, value, --> _] Replace array element at index with the float64 on the stack.
val inline stelem'r8 : Op<'x S S S, 'x>
/// [_, array, index, value, --> _] Replace array element at index with the object reference on the stack.
val inline stelem'ref : Op<'x S S S, 'x>
/// [_, obj, value --> _,] Replace the value of `field` of the object obj with value.
val inline stfld : field : FieldInfo -> Op<'x S S, 'x>
/// [_, addr, val --> _] Store value of type native int into memory at address.
val inline stind'i : Op<'x S S, 'x>
/// [_, addr, val --> _] Store value of type int8 into memory at address.
val inline stind'i1 : Op<'x S S, 'x>
/// [_, addr, val --> _] Store value of type int16 into memory at address.
val inline stind'i2 : Op<'x S S, 'x>
/// [_, addr, val --> _] Store value of type int32 into memory at address.
val inline stind'i4 : Op<'x S S, 'x>
/// [_, addr, val --> _] Store value of type int64 into memory at address.
val inline stind'i8 : Op<'x S S, 'x>
/// [_, addr, val --> _] Store value of type float32 into memory at address.
val inline stind'r4 : Op<'x S S, 'x>
/// [_, addr, val --> _] Store value of type float64 into memory at address.
val inline stind'r8 : Op<'x S S, 'x>
/// [_, addr, val --> _] Store value of type object reference into memory at address.
val inline stind'ref : Op<'x S S, 'x>
/// [_, value --> _] Pop a value from stack into local variable `local`.
val inline stloc : local : Local -> Op<'x S, 'x>
/// [_, dest, src --> _,] Store a value of type `ty` at an address.
val inline stobj : ty : Type -> Op<'x S S, 'x>
/// [_, val --> _,] Replace the value of `field` with val.
val inline stsfld : field : FieldInfo -> Op<'x S, 'x>
/// [_, value1, value2 --> _, result] Subtract value2 from value1, returning a new value.
val inline sub : Op<'x S S, 'x S>
/// [_, value1, value2 --> _, result] Subtract native value2 from value1 with overflow check, returning a new value.
val inline sub'ovf : Op<'x S S, 'x S>
/// [_, value1, value2 --> _, result] Subtract unsigned native value2 from value1 with overflow check, returning a new value.
val inline sub'ovf'un : Op<'x S S, 'x S>
/// [_, n --> _] Jump to nth label in table, or fall through to next instruction if n is out of range.
val inline switch : 'x Label seq -> Op<'x S, 'x>
/// [_, _] Mark following call as a tail call.
val inline tail : Op<'x, 'x>
/// [_, exn --> _] Throw an exception.
val inline throw : Op<'x S, 'y>
/// [_, addr --> _, addr] Subsequent pointer instruction might be unaligned.
val inline unaligned : Op<'x S, 'x S>
/// [_, obj --> _, valueTypePtr] Extract a value-type from obj, its boxed representation.
val inline unbox'val : valueTy : Type -> Op<'x S, 'x S>
/// [_, obj --> _, value or obj] Extract a value-type from obj, its boxed representation
val inline unbox'any : ty : Type -> Op<'x S, 'x S>
/// [_, addr --> _, addr] Subsequent pointer reference is volatile.
val inline volatile' : Op<'x S, 'x S>

