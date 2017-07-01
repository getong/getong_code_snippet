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
