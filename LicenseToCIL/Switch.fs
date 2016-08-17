module LicenseToCIL.Switch
open LicenseToCIL
open LicenseToCIL.Stack
open System
open System.Reflection
open System.Reflection.Emit

type private Case<'stackin, 'stackout> = int * Op<'stackin, 'stackout>

type private SwitchGroup<'stackin, 'stackout> =
    {
        Cases : Case<'stackin, 'stackout> ResizeArray
        Default : Op<'stackin, 'stackout>
    }
    member inline this.Count = this.Cases.Count
    member this.Minimum = this.Cases.[0] |> fst
    member this.Maximum = this.Cases.[this.Cases.Count - 1] |> fst
    member this.Code =
        if this.Cases.Count > 2
        then this.SwitchCode
        else this.IfElseCode
    member this.IfElseCode : Op<'stackin S, 'stackout> =
        cil {
            let! exit = Ops.deflabel
            for key, code in this.Cases do
                let! next = Ops.deflabel
                yield Ops.dup
                yield Ops.ldc'i4 key
                yield Ops.bne'un next
                yield Ops.pop
                yield code
                yield Ops.br exit
                yield Ops.mark next
            yield Ops.pop
            yield this.Default
            yield Ops.mark exit
        }
    member this.SwitchCode : Op<'stackin S, 'stackout> =
        fun _ il ->
            let exit = il.Generator.DefineLabel()
            let defaultCase = il.Generator.DefineLabel()
            let minimum = this.Minimum
            let labels = Array.zeroCreate (1 + this.Maximum - minimum)
            do
                let mutable next = 0
                for key, _ in this.Cases do
                    let k = key - minimum
                    for i = next to k - 1 do // fill gaps with branches to default
                        labels.[i] <- defaultCase
                    labels.[k] <- il.Generator.DefineLabel()
                    next <- k + 1
            (cil {
                yield Ops.ldc'i4 minimum
                yield Ops.sub
            }) null il |> ignore
            il.Generator.Emit(OpCodes.Switch, labels)
            (cil {
                let defaultCase = Ops.Label defaultCase
                let exit = Ops.Label exit
                yield Ops.br defaultCase
                for key, code in this.Cases do
                    let k = key - minimum
                    yield Ops.mark (Ops.Label (labels.[k]))
                    yield code
                    yield Ops.br exit
                yield Ops.mark defaultCase
                yield this.Default
                yield Ops.mark exit
            }) null il |> ignore
            null
            
                
// Allow a gap of one unused label in between switch cases.
let private maxSwitchGap = 1

let private switchGroupCases (cases : Case<_, _> seq) =
    seq {
        let mutable group = new ResizeArray<_>()
        let mutable expectNext = 0
        let mutable first = true
        for (i, _) as case in cases |> Seq.sortBy fst do
            if not first && expectNext + maxSwitchGap < i then
                yield group
                group <- new ResizeArray<_>()
            elif not first && i < expectNext then
                failwithf "Duplicate switch case for %d" i
            group.Add(case)
            expectNext <- i + 1       
            first <- false         
        if group.Count > 0 then yield group
    }

let private switchGroups (defaultCase : Op<_, _>) (cases : Case<_, _> seq) =
    [| for cases in switchGroupCases cases ->
        {
            Cases = cases
            Default = defaultCase
        }
    |]

type private SwitchTree<'stackin, 'stackout> =
    | Leaf of SwitchGroup<'stackin, 'stackout>
     // use the right tree if x >= pivot
    | Branch of (SwitchTree<'stackin, 'stackout> * int * SwitchTree<'stackin, 'stackout>)
    member this.Code() : Op<'stackin S, 'stackout> =
        match this with
        | Leaf group -> group.Code
        | Branch (left, pivot, right) ->
            cil {
                let! exit = Ops.deflabel
                let! gte = Ops.deflabel
                yield Ops.dup
                yield Ops.ldc'i4 pivot
                yield Ops.bge gte
                yield left.Code()
                yield Ops.br exit
                yield Ops.mark gte
                yield right.Code()
                yield Ops.mark exit
            }

let rec private switchTreeGuts (defaultCase : Op<_, _>) (groups : _ array) index count =
    if count <= 0 then Leaf { Cases = new ResizeArray<_>(); Default = defaultCase }
    elif count = 1 then
        Leaf (groups.[index])
    else
        let split = count / 2
        let left = switchTreeGuts defaultCase groups index split
        let right = switchTreeGuts defaultCase groups (index + split) (count - split)
        let pivot = groups.[index + split].Minimum
        Branch (left, pivot, right)

let private switchTree (defaultCase : Op<_, _>) (cases : Case<_, _> seq) =
    let groups = switchGroups defaultCase cases
    switchTreeGuts defaultCase groups 0 groups.Length

let cases (cases : Case<_, _> seq) (defaultCase : Op<_, _>) =
    let tree = switchTree defaultCase cases
    tree.Code()
