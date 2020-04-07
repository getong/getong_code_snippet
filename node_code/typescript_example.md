# typescript example

## literal value types

``` typescript
let restrictdValue: 1 | 2 | 3 = 3;

function getRandomValue(): 1 | 2 | 3 | 4 {
    return Math.floor(Math.random() * 4) + 1 as 1 | 2 | 3 | 4;
}
```

## optional property

``` typescript
let hat = { name: "Hat", price: 100 };
let gloves = { name: "Gloves", price: 75 };
let umbrella = { name: "Umbrella", price: 30,
        hasFeature: (feature) => feature === Feature.Waterproof };
let mirrorShades = { name: "Sunglasses", price: 54, finish: "mirrored"};
let darkShades: Product = { name: "Sunglasses", price: 54, finish: "flat"};
let products: Product[] = [hat, gloves, umbrella, mirrorShades, darkShades];
products.forEach(prod => console.log(`${prod.name}: ${prod.price} `
    + `${ prod.hasFeature ? prod.hasFeature(Feature.Waterproof) : "false" }`));
```


## types system

``` typescript
boolean
number
string
void
null
undefined
object
any
unknown
never
symbol
```

## use grapheme-splitter to split non-ascii string

## apply, call, and bind

``` typescript
function add(a: number, b: number): void {
    console.log("a + b is " + (a + b));
}

add(10, 20);
add.apply(null, [10, 20]);
add.call(null, 10, 20);
add.bind(null, 10, 20)()

```
