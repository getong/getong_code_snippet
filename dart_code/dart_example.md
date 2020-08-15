# dart example

## constructor function

``` dart
class ClassName {
    int a;
    int b;
    ClassName(this.a, this.b);
}
```

## fat arrow function

``` dart
int funcName() => 42;
```

## get and set function

``` dart
class myClass {
  String name;
  String get getName => name;
  set setName(String aValue) => name = aValue;
}

var myObject = myClass();
myObject.setName = "Sanjib";
print(myObject.getName);
```

## override
@override is not a must tag, with it can enhances the code reading.

``` dart
@override
void hello() {
    return "world";
}
```

## mixin

``` dart
class A extends B with C
// Use A class all methods and the methods that B class does not cover
class T = A with B;
```
A can inheritance B class and use C class functions.

## null check

``` dart
x ??= 3;
```

## Cascades

``` dart
"String".length.toString(); // 6
"String"..length.toString(); // "String"
```


## type

``` dart
var type_string = <String> ['a', 'b', 'c'];
print(type_string);
print("hello" + "world");
print("hello" * 2);
```

## Set, StringBuffer data type

``` dart
Set mySet = {1, 2, 3};
mySet.lookup(2);

StringBuffer stringBuffer = StringBuffer('test string');
stringBuffer.write(' append string');
```


## Future class

``` dart
import 'dart:async';

void main(){

  print("The main UI thread is starting on here.");
  Future<String> displayingNewsHeadlines = Future.delayed(Duration(seconds: 1), (){
      return "The latest headlines are displayed here after 1 second.";
  });

  displayingNewsHeadlines.then((displayString){
      print("Displaying news headlines here: $displayingNewsHeadlines, the displayString : $displayString");
  });
  print("The main UI thread ends.");

}

```
the output:

```
The main UI thread is starting on here.
The main UI thread ends.
Displaying news headlines here: Instance of 'Future<String>', the displayString : The latest headlines are displayed here after 1 second.
```
It is noted that, the Future object still runs after the end of the main function.
Some methods:

``` dart
catchError(Function onError, {bool test(Object error)}) → Future<T>
then<R>(FutureOr<R> onValue(T value), {Function onError}) → Future<R>
timeout(Duration timeLimit, {FutureOr<T> onTimeout()}) → Future<T>
whenComplete(FutureOr action()) → Future<T>
```

## runZonedGuarded function

``` dart
// 这样就可以处理所有未处理的异常了。
void main() async {
  runZonedGuarded(() => runApp(const App()));
 }
```
copy from [Flutter runZonedGuarded捕捉不到未处理错误问题的原因](https://segmentfault.com/a/1190000022892971)

## Isolate

``` dart
Isolate.spawn<T>(void entryPoint(T message), T message, {bool paused: false, bool errorsAreFatal, SendPort onExit, SendPort onError, String debugName}) → Future<Isolate>
Isolate.kill({int priority: beforeNextEvent}) → void
```

## async, await

``` dart
main() {
    getData();
    print("continue...");
}

getData() async {
    var data = await "data string";
    print(data);
}
```

## ~/ operator

``` dart
5 ~/ 2 == 2;
7 ~/ 4 == 2;
```
