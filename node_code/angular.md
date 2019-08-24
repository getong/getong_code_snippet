# angular

## new

``` shell
sudo yarn global add @angular/cli
ng new my-dream-app
ng new --style sass --commit false -g true --routing false my-dream-app
cd my-dream-app
ng serve
ng build
```
copy from [Angular CLI](https://cli.angular.io/)

## configure the package manager

``` shell
ng config -g cli.packageManager yarn
ng config -g cli.packageManager cnpm
ng config -g cli.packageManager npm
```
or change the ~/.angular-config.json

``` json
{
  "version": 1,
  "cli": {
    "packageManager": "yarn"
  }
}
```
copy from [Using Yarn with Angular CLI v6+](https://medium.com/@beeman/using-yarn-with-angular-cli-v6-7f53a7678b93)

## generate
``` shell
ng generate component users
ng generate service users
ng generate module users
ng generate directive users
ng generate class users
ng generate pipe users
```
