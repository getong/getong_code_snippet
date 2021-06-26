# canvas save and restore

``` html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8">
    <title>Title</title>
    <style>
      canvas {
          border:1px solid red;
      }
    </style>
  </head>
  <body>
    <canvas id="cvs" width="500" height="500"></canvas>
    <script>
      var cvs = document.getElementById('cvs');
      var ctx = cvs.getContext('2d');

              /*
        * 状态保存：
        * ctx.save();
        * 把当前的状态(绘制环境的所有属性(线宽、颜色、线帽等))copy一份进行保存。
        * */

              /*
        * 状态回滚(恢复)：
        * ctx.restore();
        * 把最近保存的一次状态作为当前状态。  (保存与回滚类似栈的结构)
        * */

      ctx.save();

      // 修改状态
      ctx.lineWidth = 10;
      ctx.strokeStyle = 'blue';
      ctx.lineCap = 'round';

      // 修改状态之后绘制一条线
      ctx.moveTo( 10, 10 );
      ctx.lineTo( 310, 10 );
      ctx.stroke();

      ctx.save();

      // 改状态
      ctx.lineWidth = 3;
      ctx.strokeStyle = 'pink';

      // 修改状态之后再绘制一条线
      ctx.beginPath();
      ctx.moveTo( 10, 50 );
      ctx.lineTo( 310, 50 );
      ctx.stroke();

      // 回滚
      ctx.restore();

      // 回滚之后再绘制一条线
      ctx.beginPath();
      ctx.moveTo( 10, 100 );
      ctx.lineTo( 310, 100 );
      ctx.stroke();

      // 再回滚
      ctx.restore();

      // 再回滚之后再绘制一条线
      ctx.beginPath();
      ctx.moveTo( 10, 150 );
      ctx.lineTo( 310, 150 );
      ctx.stroke();
    </script>
  </body>
  </html><!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>Title</title>
    <style>
        canvas {
            border:1px solid red;
        }
    </style>
</head>
<body>
    <canvas id="cvs" width="500" height="500"></canvas>
    <script>
        var cvs = document.getElementById('cvs');
        var ctx = cvs.getContext('2d');

        /*
        * 状态保存：
        * ctx.save();
        * 把当前的状态(绘制环境的所有属性(线宽、颜色、线帽等))copy一份进行保存。
        * */

        /*
        * 状态回滚(恢复)：
        * ctx.restore();
        * 把最近保存的一次状态作为当前状态。  (保存与回滚类似栈的结构)
        * */

        ctx.save();

        // 修改状态
        ctx.lineWidth = 10;
        ctx.strokeStyle = 'blue';
        ctx.lineCap = 'round';

        // 修改状态之后绘制一条线
        ctx.moveTo( 10, 10 );
        ctx.lineTo( 310, 10 );
        ctx.stroke();

        ctx.save();

        // 改状态
        ctx.lineWidth = 3;
        ctx.strokeStyle = 'pink';

        // 修改状态之后再绘制一条线
        ctx.beginPath();
        ctx.moveTo( 10, 50 );
        ctx.lineTo( 310, 50 );
        ctx.stroke();

        // 回滚
        ctx.restore();

        // 回滚之后再绘制一条线
        ctx.beginPath();
        ctx.moveTo( 10, 100 );
        ctx.lineTo( 310, 100 );
        ctx.stroke();

        // 再回滚
        ctx.restore();

        // 再回滚之后再绘制一条线
        ctx.beginPath();
        ctx.moveTo( 10, 150 );
        ctx.lineTo( 310, 150 );
        ctx.stroke();
    </script>
</body>
</html>
```
copy from [Canvas 状态保存(save())，状态回滚(restore())](https://blog.csdn.net/houyanhua1/article/details/79948718)
