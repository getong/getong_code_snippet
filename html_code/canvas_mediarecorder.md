# convert canvas to video


``` javascript
function record(canvas, time) {
    var recordedChunks = [];
    return new Promise(function (res, rej) {
        var stream = canvas.captureStream(25 /*fps*/);
        mediaRecorder = new MediaRecorder(stream, {
            mimeType: "video/webm; codecs=vp9"
        });

        //ondataavailable will fire in interval of `time || 4000 ms`
        mediaRecorder.start(time || 4000);

        mediaRecorder.ondataavailable = function (e) {
            recordedChunks.push(event.data);
            if (mediaRecorder.state === 'recording') {
                // after stop data avilable event run one more time
                mediaRecorder.stop();
            }

        }

        mediaRecorder.onstop = function (event) {
            var blob = new Blob(recordedChunks, {
                type: "video/webm"
            });
            var url = URL.createObjectURL(blob);
            res(url);
        }
    })
}
```
copy from [Convert HTML5 Canvas Sequence to a Video File](https://stackoverflow.com/questions/19235286/convert-html5-canvas-sequence-to-a-video-file)
