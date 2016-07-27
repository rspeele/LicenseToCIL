# What is it?

This is a library that wraps the .NET `System.Reflection.Emit` API for
generating CIL code at runtime. The wrapper adds a tiny bit of type safety
by doing two things:

## Dedicated function per opcode

Each CIL opcode gets its own function. This way, the function for
generating the `ldstr` opcode takes a `string`. This might sound
obvious, but the underlying `System.Reflection.Emit` API represents
each opcode as an enum and there is nothing stopping you from writing
`generator.Emit(OpCodes.Ldstr, 27)`.

## Type-checked stack depth

Most opcodes have a consistent stack behavior. For example, the `add`
opcode requires the there be two numbers on the stack. It will pop the
two, add them, and push the result.

License To CIL represents this stack behavior as a type, `Op<'x S S,
'x S>`, which indicates that there must be at least two elements on
the stack beforehand, and that after adding there will be one element
in their place.

The sequences of opcodes you emit must have types that "line up", or
you'll get an error compiling your F# program.

For example, these snippets will compile:

```
cil {
    yield ldc'i4 1
    yield ldc'i4 2
    yield add
    yield ldc'i4 3
    yield add
    yield ret
} |> toDelegate<Func<int>> "myFunction1"
```

```
cil {
    yield ldc'i4 1
    yield ldc'i4 2
    yield ldc'i4 3
    yield add
    yield add
    yield ret
} |> toDelegate<Func<int>> "myFunction2"
```

This one will not:

```
cil {
    yield ldc'i4 1
    yield ldc'i4 2
    yield add
    yield add // stack underflow here!
    yield ldc'i4 3
    yield ret
} |> toDelegate<Func<int>> "myFunction3"
```

Note that only the number of elements on the stack is checked, not the types of those elements.

For my use case, much of the code I was generating was working with types that weren't known
(or didn't even exist) until runtime, so it was more hassle than help to try to track them statically.

There is a [similar project](https://github.com/kbattocchi/ILBuilder)
by [kbattocchi](https://github.com/kbattocchi) that *does* track types.

# How can I use it?

## Writing CIL

The main feature of LicenseToCIL is the `cil` computation expresssion.
You can write code in here much like if you were using a `seq` expression to generate
a list of opcodes. You can use if/else and `match` expressions to generate code conditionally,
as long as the stack state ends up consistent following each possible branch:

```
let defaultOfType (ty : System.Type) =
    cil {
        if ty.IsValueType then
            let! loc = deflocal ty
            yield ldloca loc
            yield initobj ty
            yield ldloc loc
        else
            yield ldnull
    }
```

The result of each `cil` expression is an `Op<'stackin, 'stackout>`, which you can treat like an opcode.
That is, you can write a block of code in one `cil` expression, store the resulting `Op` in a variable,
and `yield` it from other CIL expressions to inline that code whereever you need it.

You can find all the CIL opcodes in the `LicenseToCIL.Ops` module, with XML comments describing their
stack transition behavior. They are mostly named identically to the CIL opcodes, except with `.` replaced by `'`,
so `ldc.i4` becomes `ldc'i4`. These are legal F# identifiers and are a bit easier to type too.

A few opcodes have been renamed to avoid collisions with F# keywords and builtins:

| CIL opcode | Ops module function |
| ---------- | ------------------- |
| unbox      | unbox'val           |
| and        | bit'and             |
| or         | bit'or              |
| xor        | bit'xor             |
| not        | bit'not             |

### Labels

When you need to generate code that branches or loops, you use labels.
In a CIL block, you can define a new label by writing `let! myLabel = deflabel`.
Then you mark where in your code that label should go with `yield mark myLabel`.

For conditional branches, the code following the label and the code following the branch instruction
need to operate on the same number of stack elements. This requirement is type-checked.

Here's an example that simply decrements its argument down to 0 in a loop:

```
let sum =
    cil {
        let! loop = deflabel
        let! exit = deflabel

        yield mark loop  // <----------------------------------------+
        yield ldarg 0    //                                          |
        yield ldc'i4 0   //                                          |
        yield ble's exit // if arg <= 0, branch to exit ----------+  |
        yield ldarg 0    //                                       |  |
        yield ldc'i4 1   //                                       |  |
        yield sub        //                                       |  |
        yield starg 0    // arg = arg - 1                         |  |
        yield br's loop  // unconditionally branch back to here --|--+
                         //                                       |
        yield mark exit  // <-------------------------------------+
        yield ret'void
    }
```

## Executing your code

If you just want to assemble `Func<_, ..., _>` or `Action<_, ..., _>` delegates,
you can use the `toDelegate` helper.

```
cil {
    yield ldarg 0
    yield ret
} |> toDelegate<Func<obj, obj>>
```

If you have a `System.Reflection.Emit.ILGenerator`, you can apply an `Op<_, _>` to it like so:

```
let writeCode (op : Op<_, _>) (il : ILGenerator) =
    op Stack.empty (IL(il))
```

Check out the examples in LicenseToCIL.Examples for more in depth usage.