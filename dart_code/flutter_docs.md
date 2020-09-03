# flutter doc

## flutter state
[Flutter 深入理解 State](https://hongruqi.github.io/2019/01/26/Flutter%20%E6%B7%B1%E5%85%A5%E7%90%86%E8%A7%A3%20State/)


## share data among widgets
[Flutter的数据传递与管理方案](https://zhuanlan.zhihu.com/p/110120429)


## Navigator
在Android中，页面对应的是Activity，在iOS中是ViewController。而在Flutter中，页面只是一个widget！
创建两个页面。
调用Navigator.push导航到第二个页面。
调用Navigator.pop返回第一个页面。
``` dart
import 'package:flutter/material.dart';

void main() {
  runApp(new MaterialApp(
    title: 'Navigation Basics',
    home: new FirstScreen(),
  ));
}

class FirstScreen extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return new Scaffold(
      appBar: new AppBar(
        title: new Text('First Screen'),
      ),
      body: new Center(
        child: new RaisedButton(
          child: new Text('Launch new screen'),
          onPressed: () {
            Navigator.push(
              context,
              new MaterialPageRoute(builder: (context) => new SecondScreen()),
            );
          },
        ),
      ),
    );
  }
}

class SecondScreen extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return new Scaffold(
      appBar: new AppBar(
        title: new Text("Second Screen"),
      ),
      body: new Center(
        child: new RaisedButton(
          onPressed: () {
            Navigator.pop(context);
          },
          child: new Text('Go back!'),
        ),
      ),
    );
  }
}
```

copy from [导航到新页面并返回](https://flutterchina.club/cookbook/navigation/navigation-basics/)
