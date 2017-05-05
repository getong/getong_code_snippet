# ejabberd_update.erl 代码解读
今天看了一下ejabberd_update.erl模块，发现ejabberd利用这个模块可以达到整个项目热更的效果。mochiweb的reloader.erl也可以达到热更全部新的模块，思路比较相似。reloader.erl会启动一个进程，每隔一秒扫描>一下整个项目目录，然后比较beam文件的版本信息，有不同就强制更新，有进程会崩溃的情况出现。这个在开发的时候比较有用，但不建议使用在实际的生产环境。

ejabberd使用了2个没有文档的模块，systools_rc.erl 以及release_handler_1.erl, 都是sasl类库中的模块。
首先获取所有要更新的beam文件列表。 通过beam_lib:version/1获取硬盘中的所有beam文件的版本信息， 然后通过Module:module_info/0来获取加
载到内存中的beam文件版本信息。这2个版本信息比较，不同的就是要更新的beam文件。

所有要更新的beam文件会逐一跟当前的属性信息比较，形成
`{update, Module, {advanced, Extra}}`,
`{update, Module, {advanced, 0}}`,
`{load_module, Module}` 等3种信息， 由systools_rc:translate_scripts/3组装好要更新的指令.

最后使用了release_handler_1:eval_script/5 函数来执行最后的更新指令。

至此，ejabberd的更新模块完成。
