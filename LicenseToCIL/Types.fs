namespace LicenseToCIL
open LicenseToCIL.Stack
open System
open System.Reflection
open System.Reflection.Emit

type IL(generator : ILGenerator) =
    member __.Generator = generator

type Op<'stackin, 'stackout> = S<'stackin> -> IL -> S<'stackout>
