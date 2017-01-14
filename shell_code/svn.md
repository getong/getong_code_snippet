#svn 操作
##创建分支

```
svn cp -m "create branch" svn://svn_server/repo/trunk svn://svn_server/repo/branches/branch_name
```

##获得分支

```
svn co --username=user svn://svn_server/repo/branches/branch_name
```

##合并主干上的最新代码到分支上

```
cd branch_name
svn merge svn://svn_server/repo/trunk
```

##如果需要预览该刷新操作，可以使用svn mergeinfo命令，如：

```
svn mergeinfo svn://svn_server/repo/trunk --show-revs eligible
```

或使用svn merge --dry-run选项以获取更为详尽的信息。

##合并版本并将合并后的结果应用到现有的分支上

```
svn -r 99:120 merge svn://svn_server/repo/trunk
```



##建立tags

```
svn copy svn://svn_server/repo/trunk svn://svn_server/repo/tags/tag-2017-01-13 -m "tag 2017-01-13"
```


##删除分支或tags

```
svn rm svn://svn_server/repo/branches/branch_name
svn rm svn://svn_server/repo/tags/tag-2017-01-13
```
