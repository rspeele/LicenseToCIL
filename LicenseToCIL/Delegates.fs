[<AutoOpen>]
module LicenseToCIL.Delegates
open LicenseToCIL.Stack
open System
open System.Reflection
open System.Reflection.Emit

let toDelegate<'del when 'del :> Delegate> name (op : Op<E, E>) =
    let del = typeof<'del>
    let invoke = del.GetMethod("Invoke")
    let returnType = invoke.ReturnType
    let parameterTypes = invoke.GetParameters() |> Array.map (fun p -> p.ParameterType)
    let meth = DynamicMethod(name, returnType, parameterTypes, restrictedSkipVisibility = true)
    op Stack.empty Stack.empty (IL(meth.GetILGenerator()))
    downcast meth.CreateDelegate(del)
    : 'del



