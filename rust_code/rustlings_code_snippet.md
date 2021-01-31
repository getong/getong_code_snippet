# rustlings code snippet

``` rust
    let mut numbers: [Option<u16>; 5] = [None; 5];
    for iter in 0..5 {
        let number_to_add: u16 = { ((iter * 1235) + 2) / (4 * 16) as u16 };

        numbers[iter as usize] = Some(number_to_add);
    }


    let optional_value = Some(String::from("rustlings"));
    if let Some(value) = optional_value {
        println!("the value of optional value is: {}", value);
    } else {
        println!("The optional value doesn't contain anything!");
    }

    let mut optional_values_vec: Vec<Option<i8>> = Vec::new();
    for x in 1..10 {
        optional_values_vec.push(Some(x));
    }

    // the pop() method will create a Some() data, and the Some(Value) is the actual value.
    while let Some(Some(value)) = optional_values_vec.pop() {
        println!("current value: {}", value);
    }
```
