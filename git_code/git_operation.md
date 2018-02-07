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
```shell
git checkout -b develop master
git pull https://github.com/other/proj develop

git checkout master
git merge --no-ff develop
git push origin master
```
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
Create a fresh, bare clone of your repository:
``` shell
git clone --bare https://github.com/user/repo.git
cd repo.git
```

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
Then push it

``` shell
git push --force --tags origin 'refs/heads/*'

```

view all the commit user and email

``` shell
git log --pretty=format:"%cn:%ce"
```

Finally, delete it.

``` shell
cd ..
rm -rf repo.git
```
Look at [Changing author info](https://help.github.com/articles/changing-author-info/) for more info.

The old cloned repo use --rebase to fetch it.

``` shell
git pull --rebase
```

## squash

``` shell
git merge branch_name --squash
```

## rebase pr

``` shell
git rebase -i HEAD~5
git rebase --abort
git rebase --continue
```

## remote show

``` shell
git remote show origin
```

## reflog
see the local branch log
``` shell
git reflog
```

## cherry-pick

``` shell
git checkout master
git cherry-pick commit_id
git cmomit
```

## show
show the commit info

``` shell
git show commit_id
```

## stash
```shell
$ git stash save filename
# stash all the changed files, no include the untracked files
$ git stash save

# stash all the changed files, include the untracked files
$ git stash save --include-untracked

# list the stash history
git stash list

# stash apply
$ git stash apply stash@{0}

# drop
$ git stash drop stash@{0}

# pop
$ git stash pop
$ git stash pop stash@{0}

```
## clean the repo
``` shell
$ git clean -d
```

## git ignore
```shell
$ cat .gitignore
*.swp
*.a
!lib.a
```
All the `.swp` files and all the `.a` file except the `lib.a` file will be ignore


## git mv

``` shell
git mv a_file b_file
git commit --dry-run -a
git commit -m 'rename'
```

## detele local branch

``` shell
$ git branch -d <branch_name>
```

## delete remote branch

``` shell
$ git push origin --delete <branch_name>
```

## delete added file from git cache, not commited

``` shell
$ git rm --cached file_name
```
The `file_name` is just added to git, not commited.


## amend, change the last commit info

``` shell
$ git add file
$ git commit --amend
```
This will append the `add file` into the last commit info.


## git commit with future date

``` shell
$ git commit --date=2017-07-11T12:25:23
```


## githug answer
githug is very suitable to master git, but with something difficult.
The answer is below, check for help.
[githug 游戏笔记](http://www.jianshu.com/p/e8e6358e81e0)
[Githug通关全攻略](http://fancyoung.com/blog/githug-cheat-sheet/)
[Githug – What is it?](https://www.tobscore.com/githug-what-is-it/)

## bisect
see [让 Git Bisect 帮助你](https://www.oschina.net/translate/letting-git-bisect-help-you)

``` shell
git bisect start
git bisect bad <commit-id>
git bisect good <commit-id>
git bisect reset
git bisect visualize
```
see [.5 Git 工具 - 使用 Git 调试](https://git-scm.com/book/zh/v1/Git-%E5%B7%A5%E5%85%B7-%E4%BD%BF%E7%94%A8-Git-%E8%B0%83%E8%AF%95)

## git concept

``` shell
workspace 工作区
index/stage 暂存区
repository 仓库区（本地仓库）
remote 远程仓库
```

## git fetch and reset to origin/master

``` shell
$ git fetch origin
$ git checkout master
$ git reset --hard origin/master
```


## reset
``` shell
$ git reset ref123
$ git reset HEAD^3
$ git diff
$ git add —all
$ git diff —-staged
```

## blame
view every line's commit message of a file
```shell
$ git blame filename
```

## tag
```shell
# make a tag
$ git tag tag_name commit_id
# show the tag info
$ git show tag_name
# get all the tag names
$ git tag
$ git push --tags
```
