module LicenseToCIL.Examples.Factorial
open System
open LicenseToCIL
open LicenseToCIL.Ops
open Microsoft.VisualStudio.TestTools.UnitTesting

// Here we will implement the factorial function a few different ways in CIL.
// First, let's define some requirements for the factorial function.
// For simplicity's sake we'll have `factorial x` return 1 when `x` is <= 1.

let testFactorial fac =
    let failed =
        [
            "x = 0", fac 0 = 1
            "x = 1", fac 1 = 1
            "x = 2", fac 2 = 2
            "x = 3", fac 3 = 6
            "x = 4", fac 4 = 24
            "x = 5", fac 5 = 120
            "x = -1", fac -1 = 1 // we will simply clamp at the minimum instead of calling negative x undefined
        ] |> List.filter (not << snd) |> List.map fst
    if List.isEmpty failed then printf "Passed tests!"
    else failwithf "Failed tests %A" failed

let rec fsRecursiveFactorial x =
    if x > 1 then x * fsRecursiveFactorial (x - 1)
    else 1

let fsIterativeFactorial x =
    let mutable acc = 1
    let mutable n = x
    while n > 1 do
        acc <- acc * n
        n <- n - 1
    acc

let example() =
    cil {
        yield ldc'i4 1
        yield ldc'i4 2
        yield add
        yield ldc'i4 3
        yield add
        yield ldc'i4 4
        yield add
        yield ldc'i4 5
        yield add
    }

let cilIterativeFactorial =
    cil {
        let! acc = deflocal typeof<int>
        let! loop = deflabel
        let! exit = deflabel

        // acc = 1
        yield ldc'i4 1
        yield stloc acc  // acc <- 1

        // while x > 1
        yield mark loop
        yield ldarg 0    // x
        yield ldc'i4 1   // x, 1
        yield ble's exit
        
        // acc <- acc * x
        yield ldloc acc  // acc
        yield ldarg 0    // acc, x
        yield mul        // acc*x
        yield stloc acc  // acc <- acc * x

        // x <- x - 1
        yield ldarg 0    // x
        yield ldc'i4 1   // x, 1
        yield sub        // x-1
        yield starg 0    // x <- x-1

        yield br's loop

        // return acc
        yield mark exit
        yield ldloc acc
        yield ret
    } |> toDelegate<Func<int, int>> "cilIterativeFactorial"
    
let cilStackIterativeFactorial =
    cil {
        let! acc = deflocal typeof<int>
        let! loop = deflabel
        let! exit = deflabel

        // acc = 1
        yield ldc'i4 1   // 1
        yield stloc acc  // acc <- 1

        // while x > 1
        yield ldarg 0    // x
        yield mark loop
        yield dup        // x, x
        yield ldc'i4 1   // x, x, 1
        yield ble's exit // x
        
        // acc <- acc * x
        yield dup        // x, x
        yield ldloc acc  // x, x, acc
        yield mul        // x, x*acc
        yield stloc acc  // x, acc <- x*acc
        
        // x <- x - 1
        yield ldc'i4 1   // x, 1
        yield sub        // x-1

        yield br's loop

        // return acc
        yield mark exit  // x
        yield pop
        yield ldloc acc
        yield ret
    } |> toDelegate<Func<int, int>> "cilIterativeFactorial"

[<TestClass>]
type TestFactorial() =
    [<TestMethod>]
    member __.TestFsRecursiveFactorial() =
        testFactorial fsRecursiveFactorial
    [<TestMethod>]
    member __.TestFsIterativeFactorial() =
        testFactorial fsIterativeFactorial
    [<TestMethod>]
    member __.TestCILIterativeFactorial() =
        testFactorial cilIterativeFactorial.Invoke
    [<TestMethod>]
    member __.TestCILStackIterativeFactorial() =
        testFactorial cilStackIterativeFactorial.Invoke