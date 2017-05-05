# git operations

## pull

```
$ git pull --rebase origin master
```

## mv

```
$ git mv dir other_dir
$ git commit -a -m 'commit msg'
$ git push
```

## github合并pr， 其中master是要合并到的主干， develop是要被合并的分支

git checkout -b develop master
git pull https://github.com/other/proj develop

git checkout master
git merge --no-ff develop
git push origin master

## can't pull new tags
Sometimes the new tag in remote repository, pull can't get the new tags. Fetch it.

``` shell
git fetch --tags
```
