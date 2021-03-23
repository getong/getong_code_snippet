# actix web


``` rust
use actix_web::{web, App, HttpResponse, HttpServer, Responder};
async fn hello() -> impl Responder {
HttpResponse::Ok().body("Hello world")
}
#[actix_web::main]
async fn main() -> std::io::Result<()> {
println!("Listening on port 8080");
HttpServer::new(|| {
App::new().route("/hello", web::get().to(hello))
})
.bind("127.0.0.1:8080")?
.run()
.await
}
```
The hello() handler is an async function that returns something
that implements a Responder trait. A Responder is something that can be
converted into an HTTP response. It’s implemented on common types like
&str, String, and u8 arrays.
