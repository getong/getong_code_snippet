# rust read write line file

## rust read line file

``` rust
use std::{
    fs::File,
    io::{prelude::*, BufReader},
    path::Path,
};

fn lines_from_file(filename: impl AsRef<Path>) -> Vec<String> {
    let file = File::open(filename).expect("no such file");
    let buf = BufReader::new(file);
    buf.lines()
        .map(|l| l.expect("Could not parse line"))
        .collect()
}

// ---

fn main() {
    let lines = lines_from_file("/etc/hosts");
    for line in lines {
        println!("{:?}", line);
    }
}
```
copy from [Read a file and get an array of strings](https://stackoverflow.com/questions/30801031/read-a-file-and-get-an-array-of-strings)

## rust write line file

``` rust
use std::fs;

fn main() {
    let v = vec![
        String::from("aaa"),
        String::from("bbb"),
        String::from("ccc")];

    fs::write("file", v.join("\n")).expect("");
}
```
copy from [Writing a Vec<String> to files using std::fs::write](https://stackoverflow.com/questions/72957085/writing-a-vecstring-to-files-using-stdfswrite/72957754)
