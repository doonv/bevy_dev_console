<!-- markdownlint-disable-next-line MD041 -->
The built-in parser provides a mini-language that can evaluate expressions, call commands, and modify resources.

<div class="warning" style="--warning-border-color: #1F6FEB">
<b>Note</b>

This guide assumes you already have used Rust before and that you know how it works.
</div>

## Basic variable assignments and math

For simplicity, the `let` keyword is not required for creating variables.

You can do most basic operations:

- Arithmetic: Addition/Subtraction/Multiplication/Division/Modulus
- Bitwise operations: AND/XOR/OR/NOT
- ~~Comparisons~~ (Coming soon-ish)

```rust
# bevy_dev_console::test_builtin_parser!({
$ x = 5 + 4
# ;
> 9 (integer)
# }{
$ x - 2
# ;
> 7 (integer)
# }{
$ x = x * -3
# ;
> -27 (integer)
# }{
$ !true
# ;
> false
# }{
$ !5u8
# ;
> 250 (u8)
# }{
$ 84 ^ 32
# ;
> 116 (integer)
# })
```

## Ownership and Borrowing

The built-in parser uses the a similar memory model to Rust's.

This means assigning a variable's value to another causes the original variable to get moved.

```rust
# bevy_dev_console::test_builtin_parser!({
$ x = "Hello World!"
# ;
> "Hello World!"
# }{
$ y = x
# ;
> "Hello World!"
# }{
$ x
# ; err "
ERROR variable `x` was moved
# " });
```

For simplicity, references are always mutable, and you can have multiple of them.

You can deference references to modify the value they are references.

```rust
# bevy_dev_console::test_builtin_parser!({
$ x = "Hi"
# ;
> "Hi"
# }{
$ y = &x
# ;
> "Hi"
# }{
$ z = &x
# ;
> "Hi"
# }{
$ *z = "Bye"
# ;
> "Bye"
# }{
$ x
# ;
> "Bye"
# }{
$ y
# ;
> "Bye"
# });
```

You can also use dereferencing to clone a variable's value.

```rust
# bevy_dev_console::test_builtin_parser!({
$ x = "Hmmm"
# }{
$ y = *x
# }{
$ z = x
# });
```

### Copy types

Certain types are `Copy`, which, like in Rust, means they are automatically copied instead of moved.
This means they never can be moved.

The copy types in the builtin parser are: `()`, all number types, and `bool`.

```rust
# bevy_dev_console::test_builtin_parser!({
$ x = 12
# ;
> 12 (integer)
# }{
$ drop x
# }{
$ x
# ;
> 12 (integer)
# });
```

## Types

Every value can have one of these types:

- **None**: The same as `()` in Rust.
- **Number**: Any type of Rust number (You can specify a type like this: `5usize`, `2.4f32`, `8f64`), along with these 2 additional types:
  - **Integer**: A generic integer of an unknown size that can get downcasted to the appropriate size when used with an integer of a known size.
  - **Float**: A generic float of an unknown size that can get downcasted to the appropriate size when used with a float of a known size.
- **Boolean**: A value that can either be `true` or `false`.
- **String**: An array of characters, the same as the [`String`] type in Rust.
- **Reference**: A mutable reference to another value.
- **Object**: A dynamic object, equivalent to [`HashMap<String, Value>`](std::collections::HashMap) in Rust.
- **StructObject**: A dynamic object, but with a name. Mostly used for enum assignments.
- **Tuple**: A dynamic tuple.
- **StructTuple**: A dynamic tuple, but with a name. Mostly used for enum assignments.
- **Resource**: A reference to a resource that can be modified.

## Functions

You can call functions by the name, followed by space separated arguments.

```rust
# bevy_dev_console::test_builtin_parser!({
$ print "Hello World!"
# }); stringify!( // TODO maybe test log output one day?
INFO "Hello World!"
# );
```

Custom native functions can be created and added, see the [`custom_functions` example](https://github.com/doonv/bevy_dev_console/blob/master/examples/custom_functions.rs) for an example.

## Resource Modification

You can modify [`Resource`]s that implement the [`Reflect`] trait by their name. See the [`resources` example](https://github.com/doonv/bevy_dev_console/blob/master/examples/resources.rs) for some resources to play around with.

**Warning:** Error handling for this is not finished, expect panics.

```rust,ignore
MyStructResource.number1 = 100
MyEnumResource = Option2
MyEnumResource = StructVariant { a: 5.2, b: "Hi!" }
MyEnumResource = TupleVariant(false, 5u64)
```

## Entity Queries and Modification

[*Coming soon...*](https://github.com/doonv/bevy_dev_console/issues/3)

## Loops

[*Coming soon...*](https://github.com/doonv/bevy_dev_console/issues/8)

## Functions/Closures

[*Coming soon...*](https://github.com/doonv/bevy_dev_console/issues/12)

## Boolean comparisons

*Coming soon...*

## The end, for now

`bevy_dev_console` is still in active development, and more features will be added soon.
