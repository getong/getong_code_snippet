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

## get and set

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
```
A can inheritance B class and use C class functions.

## null check

``` dart
x ??= 3;
```
