# setting up typescript

just copy from [Setting Up a New TypeScript Project](https://alligator.io/typescript/new-project/)

## init the project
``` shell
mkdir ts_project
cd ts_project
yarn add typescript --dev
./node_modules/.bin/tsc --init
```
## edit the tsconfig.json file, enable some compileroptions
the file before:

``` typescript
{
  "compilerOptions": {
    "target": "es5",
    "module": "commonjs",
    "strict": true
  }
}
```
change the file to be :

``` typescript
{
  "compilerOptions": {
    "target": "es5",
    "module": "commonjs",
    "strict": true,
    "outDir": "dist",
    "rootDir": "./src",
    "sourceMap": true
  }
}
```

## mkdir the src directory and make a source file

``` shell
mkdir src
touch src/index.ts
```
the src/index.ts is

``` typescript
const world = '🗺️';

export function hello(word: string = world): string {
  return `Hello ${world}! `;
}
```

## compile the typescript source

``` shell
./node_modules/.bin/tsc
```

## tslint

``` shell
yarn add tslint --dev
./node_modules/.bin/tslint --init
```
## change the tslint.json file
the tslint.json file before

``` typescript
{
  "defaultSeverity": "error",
  "extends": [
    "tslint:recommended"
  ],
  "jsRules": {},
  "rules": {},
  "rulesDirectory": []
}
```
edit the file to be:

``` typescript
{
    "defaultSeverity": "error",
    "extends": [
        "tslint:recommended"
    ],
    "jsRules": {},
    "rules": {
        "eofline": false,
        "quotemark": [
            true,
            "single"
        ]
    },
    "rulesDirectory": []
}
```

## use the tslint-config-airbnb

``` shell
yarn add tslint-config-airbnb --dev
```
and edit the tslint.json to be:

``` typescript
{
    "defaultSeverity": "error",
    "extends": "tslint-config-airbnb",
    "jsRules": {},
    "rules": {
        "eofline": false
    },
    "rulesDirectory": []
}
```

## gts

``` shell
yarn add gts --dev
./node_modules/.bin/gts init
./node_modules/.bin/gts check index.ts
./node_modules/.bin/gts check one.ts two.ts three.ts
./node_modules/.bin/gts check *.ts
```
the gts will regenerate the `package.json` file, and reinstall the packages:

``` shell
yarn
```
and you can also change the tslint.json

``` typescript
{
  "defaultSeverity": "error",
  "extends": [
    "./node_modules/gts/tslint.json"
  ],
  "jsRules": {},
  "rules": {},
  "rulesDirectory": []
}
```

## general development step by step

``` shell
tsc
node output/index.js

```

## add types example

``` shell
yarn add lowdb @types/lowdb
```

## allowJs

``` javascript
{
    "compilerOptions": {
    ...
    "allowJs": true
    ...
    }
}
```
