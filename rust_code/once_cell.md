# once_cell

## once_cell and sea-orm

``` rust
use sea_orm::{entity::prelude::DatabaseConnection, ConnectOptions, Database};
use tokio::sync::OnceCell;
//  异步初始化数据库
pub static DB: OnceCell<DatabaseConnection> = OnceCell::const_new();

pub async fn db_conn() -> DatabaseConnection {
    let mut opt = ConnectOptions::new(CFG.database.link.to_owned());
    opt.max_connections(1000)
        .min_connections(5)
        .connect_timeout(Duration::from_secs(8))
        .idle_timeout(Duration::from_secs(8))
        .sqlx_logging(false);
    let db = Database::connect(opt).await.expect("数据库打开失败");
    tracing::info!("Database connected");
    db
}
```
usage:

``` rust
use db::{
    db_conn,
    DB,
};

pub async fn get_by_id(Query(req): Query<SearchReq>) -> Res<Vec<sys_api_db::Model>> {
    let db = DB.get_or_init(db_conn).await;
    let res = service::sys_api_db::get_by_id(db, &req.api_id).await;
    match res {
        Ok(x) => Res::with_data(x),
        Err(e) => Res::with_err(&e.to_string()),
    }
}
```

copy from [wgr_axum_admin](https://github.com/liweilijie/wgr_axum_admin)
