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
```
