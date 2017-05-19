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

## push more than one repositories at one time
change the .git/config like below:

``` shell
[remote "origin"]
	url = https://github.com/user/abc
	url = https://gitlab.com/other_user/abc
```


## change remote repo commit user and email

``` shell
#!/bin/sh

git filter-branch --env-filter '
OLD_EMAIL="wrong@host"
CORRECT_NAME="user"
CORRECT_EMAIL="user@host.com"
if [ "$GIT_COMMITTER_EMAIL" = "$OLD_EMAIL" ]
then
    export GIT_COMMITTER_NAME="$CORRECT_NAME"
    export GIT_COMMITTER_EMAIL="$CORRECT_EMAIL"
fi
if [ "$GIT_AUTHOR_EMAIL" = "$OLD_EMAIL" ]
then
    export GIT_AUTHOR_NAME="$CORRECT_NAME"
    export GIT_AUTHOR_EMAIL="$CORRECT_EMAIL"
fi
' --tag-name-filter cat -- --branches --tags

```
The push it

``` shell
git push --force
```

view all the commit user and email

``` shell
git log --pretty=format:"%cn:%ce"
```
