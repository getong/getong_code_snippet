# rust macro

## macro pattern

```
A macro defined with macro_rules! works entirely by pattern matching. The body of
a macro is just a series of rules:
( pattern1 ) => ( template1 );
( pattern2 ) => ( template2 );
```

## macro fragement type

```
expr An expression: 2 + 2, "udon", x.len() => , ;

stmt An expression or declaration, not including any trailing semicolon (hard to use; try expr or block instead) => , ;

ty A type: String, Vec<u8>, (&str, bool), dyn Read + Send => , ; = | { [ : > as where

path A path (discussed on page 183): ferns, ::std::sync::mpsc => , ; = | { [ : > as where

pat A pattern (discussed on page 239): _, Some(ref x) => , = | if in

item An item (discussed on page 138): struct Point { x: f64, y: f64 }, mod ferns; Anything

block A block (discussed on page 137): { s += "ok\n"; true } Anything

meta The body of an attribute (discussed on page 191): inline, derive(Copy, Clone), doc="3D models." Anything

ident An identifier: std, Json, longish_variable_name Anything

literal A literal value: 1024, "Hello, world!", 1_000_000f64 Anything

lifetime A lifetime: 'a, 'item, 'static Anything

vis A visibility specifier: pub, pub(crate), pub(in module::submodule) Anything

tt A token tree (see text): ;, >=, {}, [0 1 (+ 0 1)] Anything
```
