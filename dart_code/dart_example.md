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
