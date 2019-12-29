# gpg verify file
The `.sig`, `.asc` file is the signature file, use the `gpg` command line the verify it.
## get the key

```
gpg --keyserver keyserver.ubuntu.com --recv-keys  03D6E495
```

## verify the file

```
gpg --verify julia-0.5.1-full.tar.gz.asc julia-0.5.1-full.tar.gz
```

## restart the gpg-agent

```
gpgconf --kill gpg-agent
```
## gpg
```
## gpg 2.1.17 above, generate a gpg key
gpg --full-generate-key

gpg --default-new-key-algo rsa4096 --gen-key

## list gpg keys
gpg --list-secret-keys --keyid-format LONG
gpg --list-secret-keys

## export key
gpg --armor --export pub-GPG-KEY-ID
```
