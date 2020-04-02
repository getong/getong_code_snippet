# yarn

## common usage

```
## always use the init command to init your typescript project
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
for i in parcel-bundler typescript @angular/cli ts-node gulp-cli webpack prettier tsun @types/node mocha nodemon phantom inquirer tsc-watch jest @types/jest ts-jest layaair2-cmd
do
yarn global add $i
done
```

## config set prefix

``` shell
yarn config set prefix ~/.yarn
yarn global bin
echo 'export PATH=~/.yarn/bin:$PATH' >> ~/.zshrc
yarn global dir
```

## install yarn by using apt

``` shell
curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | sudo apt-key add -
echo "deb https://dl.yarnpkg.com/debian/ stable main" | sudo tee /etc/apt/sources.list.d/yarn.list
sudo apt update && sudo apt install --no-install-recommends yarn
```

## nodejs v13.12.0 does not work on some old cpu
see [zlib Illegal Instruction on CPU without SSSE3 support](https://github.com/nodejs/node/issues/32553)
