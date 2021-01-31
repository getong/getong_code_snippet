# rustlings code snippet

``` rust
    let mut numbers: [Option<u16>; 5] = [None; 5];
    for iter in 0..5 {
        let number_to_add: u16 = { ((iter * 1235) + 2) / (4 * 16) as u16 };

        numbers[iter as usize] = Some(number_to_add);
    }
```
