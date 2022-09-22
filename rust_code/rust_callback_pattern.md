# rust callback pattern

## callback optional

``` rust
#[async_trait(?Send)]
trait Foo {
    async fn update(&self, item: String) -> Result<String, Error>;

    async fn update_all(
        &self,
        items: Vec<String>,
        // cb: &mut dyn FnMut(String),
        cb: Option<&mut dyn FnMut(String)>,
    ) -> Result<Vec<String>, Error> {
        let cb = cb.unwrap_or(&mut |_| ());
        let cb = RefCell::new(cb);
        let cb = &cb;
        stream::iter(items)
            // .map(move |item| async move { self.update(item).await })
            .map(move |item| async move {
                // cb(item.clone());
                // cb.borrow_mut()(item.clone());
                // cb.borrow_mut().map(|cb| cb(item.clone()));
                cb.borrow_mut().as_mut().map(|cb| cb(item.clone()));
                self.update(item).await
            })
            .buffered(3)
            .try_collect()
            .await
    }
}


struct FooImpl;
#[async_trait(?Send)]
impl Foo for FooImpl {
    async fn update(&self, item: String) -> Result<String, Error> {
        println!("{:?} Updating {}", time(), item);
        tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;
        Ok(item)
    }
}

struct FooWrapper {
    foo: Box<dyn Foo>,
}
impl FooWrapper {
    async fn do_all_the_things(&mut self) -> Result<(), Error> {
        let items = (0..10).into_iter().map(|i| format!("Item {}", i)).collect();
        let mut cb = |item| self.updates.notify(item);
        // self.foo.update_all(items, &mut cb).await?;
        self.foo.update_all(items, Some(&mut cb)).await?;
        Ok(())
    }
}


struct Updates;
impl Updates {
    fn notify(&mut self, item: String) {
        println!("{:?} About to update {}", time(), item);
    }
}

struct FooWrapper {
    foo: Box<dyn Foo>,
    updates: Updates,
}
```

copy from [An Unfortunate Experience with Rust](https://blog.polybdenum.com/2022/06/25/an-unfortunate-experience-with-rust.html)
