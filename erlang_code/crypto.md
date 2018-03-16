# crypto module

## supports
Can be used to determine which crypto algorithms that are supported by the underlying OpenSSL library

``` erlang
1> crypto:supports().
[{hashs,[sha,sha224,sha256,sha384,sha512,md4,md5,ripemd160]},
{ciphers,[des3_cbc,des_ede3,des3_cbf,des3_cfb,aes_cbc,
aes_cbc128,aes_cfb8,aes_cfb128,aes_cbc256,aes_ctr,aes_ecb,
aes_gcm,aes_ige256,des_cbc,des_cfb,des_ecb,blowfish_cbc,
blowfish_cfb64,blowfish_ofb64,blowfish_ecb,rc2_cbc,rc4,
chacha20_poly1305]},
{public_keys,[rsa,dss,dh,ec_gf2m,ecdsa,ecdh,srp]},
{macs,[hmac,cmac]}]
```

## crypto hash digest
crypto support many hash digest algorithms in a simple and almost the same way.
Just test in 19.

```
hash_algorithms() =  md5 | ripemd160 | sha | sha224 | sha256 | sha384 | sha512

hash_init(Type) -> Context
	Type = md4 | hash_algorithms()

hash_update(Context, Data) -> NewContext
	Data = iodata()

hash_final(Context) -> Digest
	Digest = binary()

```
And also the hmac algorithms:

```
hmac_init(Type, Key) -> Context
	Type = hash_algorithms() - except ripemd160
	Key = iodata()
	Context = binary()

hmac_update(Context, Data) -> NewContext
	Type = hash_algorithms() - except ripemd160
	Key = iodata()
	Context = binary()

hmac_final(Context) -> Mac
	Context = Mac = binary()

```
example:
copy from tsung uuid.erl

```
sha(Namespace, Name) ->
	Context = crypto:sha_update(crypto:sha_update(crypto:sha_init(), namespace(Namespace)), Name),
	U = crypto:sha_final(Context),
	format_uuid(U, 5).

```
Note that, sha_init, sha_update, sha_final and other functions are suggested to be replaced with hash_* functions.
