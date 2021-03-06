# npm commands

## init
init a project
``` shell
mkdir dir_name
cd dir_name
npm init
```

## install
install a package

``` shell
npm i phoenix
```

## optional cmd in the package.json

``` shell
"scripts": {
    "build": "ember build",
    "lint:hbs": "ember-template-lint .",
    "lint:js": "eslint .",
    "start": "ember server",
    "test": "ember test",
    "test:all": "ember try:each",
    "semantic-release": "semantic-release"
  },
```
and you can use `start`, `test` and other commands in the scripts:

``` shell
npm start
npm test
```

## install typescript

``` shell
sudo npm install -g typescript
sudo npm install -g @angular/cli
sudo npm install -g yarn
sudo npm install -g npm
sudo npm install -g ts-node
sudo npm install -g gulp-cli
sudo npm install -g webpack
```
## taobao mirror

``` shell
npm set registry https://r.npm.taobao.org # 注册模块镜像
npm set disturl https://npm.taobao.org/dist # node-gyp 编译依赖的 node 源码镜像

## 查看npm当前镜像源
npm config get registry
## 设置npm镜像源为淘宝镜像
npm config set registry https://registry.npm.taobao.org/

## 查看yarn当前镜像源
yarn config get registry
## 设置yarn镜像源为淘宝镜像
yarn config set registry https://registry.npm.taobao.org/

## 以下选择添加
npm set sass_binary_site https://npm.taobao.org/mirrors/node-sass # node-sass 二进制包镜像
npm set electron_mirror https://npm.taobao.org/mirrors/electron/ # electron 二进制包镜像
npm set puppeteer_download_host https://npm.taobao.org/mirrors # puppeteer 二进制包镜像
npm set chromedriver_cdnurl https://npm.taobao.org/mirrors/chromedriver # chromedriver 二进制包镜像
npm set operadriver_cdnurl https://npm.taobao.org/mirrors/operadriver # operadriver 二进制包镜像
npm set phantomjs_cdnurl https://npm.taobao.org/mirrors/phantomjs # phantomjs 二进制包镜像
npm set selenium_cdnurl https://npm.taobao.org/mirrors/selenium # selenium 二进制包镜像
npm set node_inspector_cdnurl https://npm.taobao.org/mirrors/node-inspector # node-inspector 二进制包镜像

npm cache clean --force # 清空缓存
```
copy from [npm 淘宝镜像配置](https://gist.github.com/52cik/c1de8926e20971f415dd)
copy from [npm，yarn如何查看源和换源](https://zhuanlan.zhihu.com/p/35856841)

## ts-node
``` shell
ts-node ts_source_file
# repl
ts-node
>
```
