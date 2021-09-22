# rust Box Error

## Best practice to return a Result<_, Box<dyn Error>>
copy from [Best practice to return a Result<_, impl Error> and not a Result<_, &str> in Rust?](https://stackoverflow.com/questions/54159232/best-practice-to-return-a-result-impl-error-and-not-a-result-str-in-rus)

``` rust
use std::error::Error;
use std::fmt;

// Error for case where file contains '127.0.0.1'
#[derive(Debug)]
pub struct AddressIsLocalhostError;

// Display implementation is required for std::error::Error.
impl fmt::Display for AddressIsLocalhostError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Address is localhost")
    }
}

impl Error for AddressIsLocalhostError {} // Defaults are okay here.

// Now we have a function that takes a path and returns
// non-localhost Ipv4Addr on success.
// On fail it can return either of IoError, AddrParseError or AddressIsLocalhostError.
fn non_localhost_ipv4_from_file(path: &Path) -> Result<Ipv4Addr, Box<dyn Error + 'static>> {
    // Opening and reading file may cause IoError.
    // ? operator will automatically convert it to Box<dyn Error + 'static>.
    // (via From trait implementation)
    // This way concrete type of error is "erased": we don't know what's
    // in a box, in fact it's kind of black box now, but we still can call
    // methods that Error trait provides.
    let content = fs::read_to_string(path)?;

    // Parsing Ipv4Addr from string [slice]
    // may cause another error: AddressParseError.
    // And ? will convert it to to the same type: Box<dyn Error + 'static>
    let addr: Ipv4Addr = content.parse()?;

    if addr == Ipv4Add::new(127, 0, 0, 1) {
        // Here we perform manual conversion
        // from AddressIsLocalhostError
        // to Box<dyn Error + 'static> and return error.
        return Err(AddressIsLocalhostError.into());
    }

    // Everyhing is okay, returning addr.
    Ok(Ipv4Addr)
}
```
