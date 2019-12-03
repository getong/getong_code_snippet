# typescript example

## literal value types

``` typescript
let restrictdValue: 1 | 2 | 3 = 3;

function getRandomValue(): 1 | 2 | 3 | 4 {
    return Math.floor(Math.random() * 4) + 1 as 1 | 2 | 3 | 4;
}
```
