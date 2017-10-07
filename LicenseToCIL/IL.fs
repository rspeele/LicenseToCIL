namespace LicenseToCIL
open System
open System.Collections.Generic
open System.Reflection.Emit
open System.Reflection

type IL(generator : ILGenerator) =
    let freeLocals = Dictionary<Type, LocalBuilder Queue>()
    member __.MarkLabel(label : Label) =
        generator.MarkLabel(label)
    member __.EmitLabel(op : OpCode, label : Label) =
        generator.Emit(op, label)
    member __.Emit(op : OpCode) =
        generator.Emit(op)
    member __.EmitField(op : OpCode, arg : FieldInfo) =
        generator.Emit(op, arg)
    member __.EmitSwitch(labels : Label array) =
        generator.Emit(OpCodes.Switch, labels)
    member __.EmitTy(op : OpCode, arg : Type) =
        generator.Emit(op, arg)
    member __.EmitCtor(op : OpCode, arg : ConstructorInfo) =
        generator.Emit(op, arg)
    member __.EmitSByte(op : OpCode, arg : sbyte) =
        generator.Emit(op, arg)
    member __.EmitByte(op : OpCode, arg : byte) =
        generator.Emit(op, arg)
    member __.EmitInt16(op : OpCode, arg : int16) =
        generator.Emit(op, arg)
    member __.EmitInt32(op : OpCode, arg : int32) =
        generator.Emit(op, arg)
    member __.EmitInt64(op : OpCode, arg : int64) =
        generator.Emit(op, arg)
    member __.EmitSingle(op : OpCode, arg : single) =
        generator.Emit(op, arg)
    member __.EmitDouble(op : OpCode, arg : double) =
        generator.Emit(op, arg)
    member __.EmitString(op : OpCode, arg : string) =
        generator.Emit(op, arg)
    member __.EmitMethod(op : OpCode, arg : MethodInfo) =
        generator.Emit(op, arg)
    member __.DefineLabel() =
        generator.DefineLabel()
    member __.DeclareLocal(ty) =
        generator.DeclareLocal(ty)
    member __.AllocateLocal(ty : Type) =
        let free =
            let succ, free = freeLocals.TryGetValue(ty)
            if succ then free else
            let free = new Queue<LocalBuilder>()
            freeLocals.Add(ty, free)
            free
        let given =
            if free.Count > 0 then free.Dequeue()
            else generator.DeclareLocal(ty)
        given, fun () -> free.Enqueue(given)


        
