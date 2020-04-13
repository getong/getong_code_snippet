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
