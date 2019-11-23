# javascript

## make http request

``` javascript
const Http = new XMLHttpRequest();
const url='https://jsonplaceholder.typicode.com/posts';
Http.open("GET", url);
Http.send();

Http.onreadystatechange = (e) => {
  console.log(Http.responseText)
}
```
more on [Here are the most popular ways to make an HTTP request in JavaScript](https://www.freecodecamp.org/news/here-is-the-most-popular-ways-to-make-an-http-request-in-javascript-954ce8c95aaa/)

## From Callbacks, to Promises, to Async/Await
[Async JavaScript: From Callbacks, to Promises, to Async/Await](https://tylermcginnis.com/async-javascript-from-callbacks-to-promises-to-async-await/)
[异步JavaScript的演化史：从回调到Promise再到Async/Await](https://mp.weixin.qq.com/s/V6EtgI_mAFk7FAFPSVFg4Q)


## MDN JavaScript page
[MDN JavaScript](https://developer.mozilla.org/en-US/docs/Web/javascript)

## code example

``` javascript
typeof(null) === 'object'
Number("100")
String(100)
let a = NaN
typeof(a) == 'number'
100 + undefined === NaN

let names = ["Hat", "Boots", "Gloves"];
let prices = [];
let combinedArray = [...names, ...prices];
```

## prettier

``` shell
prettier --check  --write css/* js/*
```
