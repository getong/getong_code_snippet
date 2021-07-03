# rust global pool

## global r2d2 pool
copy from [How do I keep a global Postgres connection?](https://stackoverflow.com/questions/63150183/how-do-i-keep-a-global-postgres-connection)

``` rust
use r2d2_postgres::postgres::{NoTls, Client};
use r2d2_postgres::PostgresConnectionManager;

#[macro_use]
extern crate lazy_static;

lazy_static! {
    static ref POOL: r2d2::Pool<PostgresConnectionManager<NoTls>> = {
        let manager = PostgresConnectionManager::new(
            // TODO: PLEASE MAKE SURE NOT TO USE HARD CODED CREDENTIALS!!!
            "host=localhost user=postgres password=password".parse().unwrap(),
            NoTls,
        );
        r2d2::Pool::new(manager).unwrap()
    };
}



pub fn get_player(id: i32) {
    // Use global client connection object:
    let mut client = POOL.get().unwrap();
    for row in client.query("SELECT * FROM public.\"User\" WHERE \"accountID\"=$1;",&[&id]).unwrap(){
        let id: i32 = row.get(0);
        let name: &str = row.get(1);

        println!("found player: {} {}", id, name);
    }
}
```
