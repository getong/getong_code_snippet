# svn 操作
## 创建分支

```
svn cp -m "create branch" svn://svn_server/repo/trunk svn://svn_server/repo/branches/branch_name
```

## 获得分支

```
svn co --username=user svn://svn_server/repo/branches/branch_name
```

## 合并主干上的最新代码到分支上

```
cd branch_name
svn merge svn://svn_server/repo/trunk
```

## 如果需要预览该刷新操作，可以使用svn mergeinfo命令，如：

```
svn mergeinfo svn://svn_server/repo/trunk --show-revs eligible
```

或使用svn merge --dry-run选项以获取更为详尽的信息。

## 合并版本并将合并后的结果应用到现有的分支上

```
svn -r 99:120 merge svn://svn_server/repo/trunk

// restore the N version file.txt into local svn directory, and commit into remote svn server.
svn merge -rHEAD:N file.ext
```
回滚代码, 反向合并

``` shell
svn merge -r 20:10 file_or_dir
```


## 建立tags

```
svn copy svn://svn_server/repo/trunk svn://svn_server/repo/tags/tag-2017-01-13 -m "tag 2017-01-13"
```


## 删除分支或tags

```
svn rm svn://svn_server/repo/branches/branch_name
svn rm svn://svn_server/repo/tags/tag-2017-01-13
```

## svn add all the deps, including new added files and remove files

```
$ svn add --force _build
$ svn commit -m 'update new dep' _build
```

## svn 设置文件可执行权限

``` shell
$ svn propset svn:executable on filename
$ svn commit -m 'add executable' filename
```

## 获取某个版本号对应的分支
``` shell
$ svn co svn://svn_server/repo/branches/branch_name -r version_num
```

## Undo that SVN add

``` shell
svn revert --recursive example_folder
```
see [Undo that SVN add](http://data.agaric.com/undo-svn-add)

## force vimdiff long line wrap

``` vim-script
au VimEnter * if &diff | execute 'windo set wrap' | endif
```

## use vimdiff as the vim diff tool
add the diffwrap.sh to /usr/local/bin/
``` shell
#!/bin/sh

# Configure your favorite diff program here.
DIFF="/usr/bin/vimdiff"

# Subversion provides the paths we need as the sixth and seventh
# parameters.
LEFT=${6}
RIGHT=${7}

# Call the diff command
$DIFF $LEFT $RIGHT
```
then add executeable flag to it:

``` shell
chmod a+x diffwrap.sh
```

add the diff-cmd to ~/.subversion/config

```
diff-cmd = /usr/local/bin/diffwrap.sh
```
use it:

``` shell
svn diff
```
copy from [Improving svn diff with the Power of Vim](http://www.coreymaynard.com/blog/improving-svn-diff-with-the-power-of-vim/)

## svn checksum error

``` shell
cd svn_dir
svn update --set-depth empty
svn update --set-depth infinity
```
copy from [svn错误：更新源码出现校验和不匹配问题](https://blog.csdn.net/yyqasg/article/details/78788011)

## svn checkout version

``` shell
查看指定文件的历史版本

svn log filename -v -l 8 //查看指定文件最近8个版本详细信息

恢复指定文件到指定版本

svn up -r r5657 filename //恢复指定文件到版本 r5657
```
copy from [SVN恢复某个文件到特定版本](https://my.oschina.net/HeAlvin/blog/1522776)
