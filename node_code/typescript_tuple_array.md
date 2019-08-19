# typescript tuple and array

```
[string] = Tuple (fixed size array)
string[] = Array (most common array)
Array = Array (same as the 2nd but preferred if you need different types in the array)
```
example:

``` typescript
let x: [string, number];
x = ["hello", 10];

let x: string[];
x = ["hello", "world"]

let x: Array<string | number>
x = ["hello", "world", 2]
let inventory: Array<Boat, SpaceShip, Wagon>

type Vehicle = Boat | SpaceShip | Wagon
let inventory: [Vehicle]
```
in short:

```
use the Array<string | number> version if you don't know how many items will be in your Array. Use the tuple [string, number] version for small arrays with a fixed number of items that always have the same type in the order specified.
```

copy from [Difference Between Array<Type>, Type[], [Type] in TypeScript](https://mattferderer.com/difference-between-array-types-in-typescript)
