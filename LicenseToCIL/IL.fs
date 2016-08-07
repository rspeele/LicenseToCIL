namespace LicenseToCIL
open LicenseToCIL.Stack
open System
open System.Collections.Generic
open System.Reflection
open System.Reflection.Emit

type IL(generator : ILGenerator) =
    let freeLocals = Dictionary<Type, LocalBuilder Queue>()
    member __.Generator = generator
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


        
