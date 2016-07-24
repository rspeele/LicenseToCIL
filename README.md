# What is it?

This is a library that wraps the .NET `System.Reflection.Emit` API for
generating CIL code at runtime. The wrapper adds a tiny bit of type safety
by doing two things:

## Dedicated function per opcode

Each CIL opcode gets its own function. This way, the function for
generating the `ldtoken` opcode takes a `Type`. This might sound
obvious, but the underlying `System.Reflection.Emit` API represents
each opcode as an enum and there is nothing stopping you from writing
`generator.Emit(OpCodes.LdToken, "garbage")`.

## Type-checked stack arity

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
}
```

```
cil {
    yield ldc'i4 1
    yield ldc'i4 2
    yield ldc'i4 3
    yield add
    yield add
}
```

This one will not:

```
cil {
    yield ldc'i4 1
    yield ldc'i4 2
    yield add
    yield add // stack underflow here!
    yield ldc'i4 3
}
```




