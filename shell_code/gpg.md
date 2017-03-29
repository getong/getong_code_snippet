#gpg verify file
The `.sig`, `.asc` file is the signature file, use the `gpg` command line the verify it.
## get the key

```
gpg --keyserver keyserver.ubuntu.com --recv-keys  03D6E495
```

## verify the file

```
gpg --verify julia-0.5.1-full.tar.gz.asc julia-0.5.1-full.tar.gz
```
