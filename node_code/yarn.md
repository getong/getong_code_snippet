# yarn

## common usage

```
yarn init
yarn add [package]
yarn add [package]@[version]
yarn add [package]@[tag]

## 分别添加到 devDependencies、peerDependencies 和 optionalDependencies：
yarn add [package] --dev
yarn add [package] --peer
yarn add [package] --optional

## upgrade
yarn upgrade [package]
yarn upgrade [package]@[version]
yarn upgrade [package]@[tag]

## remove
yarn remove [package]

## install
yarn
## or
yarn install
```
copy from [使用](https://yarnpkg.com/zh-Hans/docs/usage)
## add

```
cd assets
yarn add vue vue-template-compiler vuedraggable
yarn add --dev node-sass sass-loader vue-loader vue-style-loader
```

## comman packages

``` shell
yarn global add parcel-bundler typescript @angular/cli ts-node gulp-cli webpack
```
