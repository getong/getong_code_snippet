# rust shared state

## global shared state crate
[once_cell](https://github.com/matklad/once_cell)
[lazy-static](https://github.com/rust-lang-nursery/lazy-static.rs)

once_cell might be better than lazy-static, as tokio adopts once_cell.

## nightly sync::{OnceLock, LazyLock}
only in rust nightly.

Note that, cell::{OnceCell, LazyCell} struct have the same api as the {OnceLock, LazyLock}, but they are !sync.


## thread_local!
Macro std::thread_local is only in current thread, not global.
