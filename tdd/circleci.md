# circleci test
## cd cmd does not change directory

```
each separate command is starting over in a new process, so if you want to change directory and then execute a command, then you would put it into the same line with &&, e.g.: cd /home/ubuntu/honoriety/docker && pwd.
```
see [Unable to change present directory or “cd” cmd ignored](https://discuss.circleci.com/t/unable-to-change-present-directory-or-cd-cmd-ignored/12537)
