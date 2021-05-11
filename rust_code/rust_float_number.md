# rust float number

## check equal

``` rust
    let x = 1.2331f64;
    let y = 1.2332f64;
    //can not use  `y != x` to compare two float number
    if (y - x).abs() < std::f64::EPSILON {
        println!("Success!");
    }
```

## divide

``` rust
fn average(values: &[f64]) -> f64 {
    let total = values.iter().fold(0.0, |a, b| a + b);
    (total / (values.len() as f64)) as f64
}
```
