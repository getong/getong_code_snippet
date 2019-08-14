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


## base58 libp2p

``` erlang
-spec base58check_encode(binary(), binary()) -> string().
base58check_encode(Version, Payload) ->
  VPayload = <<Version/binary, Payload/binary>>,
  <<Checksum:4/binary, _/binary>> = crypto:hash(sha256, crypto:hash(sha256, VPayload)),
  Result = <<VPayload/binary, Checksum/binary>>,
  base58:binary_to_base58(Result).

-spec base58check_decode(string()) -> {'ok',<<_:8>>,binary()} | {error,bad_checksum}.
base58check_decode(B58) ->
  Bin = base58:base58_to_binary(B58),
  PayloadSize = byte_size(Bin) - 5,
  <<Version:1/binary, Payload:PayloadSize/binary, Checksum:4/binary>> = Bin,
  %% validate the checksum
  case crypto:hash(sha256, crypto:hash(sha256, <<Version/binary, Payload/binary>>)) of
    <<Checksum:4/binary, _/binary>> ->
      {ok, Version, Payload};
    _ ->
      {error, bad_checksum}
  end.
```
The aeternity project also uses base58 for performance reason. base58 might be better on some areas.

## aes128 + pkcs#7填充

``` erlang
encode(Bin) ->
    Len = erlang:size(Bin),
    Value = 16 - (Len rem 16),
    PadBin = binary:copy(<<Value>>, Value),
    EncodeB = crypto:block_encrypt(aes_cbc128, ?AES_KEY, ?AES_IV, <<Bin/binary, PadBin/binary>>),
    base64:encode(EncodeB).


decode(Bin) ->
    Bin1 = base64:decode(Bin),
    case erlang:size(Bin1) rem 16 of
        0 ->
            Bin2 = crypto:block_decrypt(aes_cbc128, ?AES_KEY, ?AES_IV, Bin1),
            binary:part(Bin2, {0, byte_size(Bin2) - binary:last(Bin2)});
        _ ->
            {error, 1102}
    end.
```
copy from [erlang aes加密](https://www.jianshu.com/p/90d2fe44f6fe)

## gcm

``` elixir
# Gen once (see also https://hexdocs.pm/plug/Plug.Crypto.KeyGenerator.html#content)
k = :crypto.strong_rand_bytes(32)

# Gen every time you encrypt a message
iv = :crypto.strong_rand_bytes(32)
{ct, tag} = :crypto.block_encrypt(:aes_gcm, k, iv, {"AES128GCM", msg})
payload = Base.encode16(iv <> tag <> ct)

## decrypt
<<iv::binary-32, tag::binary-16, ct::binary>> = Base.decode16!(payload)
:crypto.block_decrypt(:aes_gcm, k, iv, {"AES128GCM", ct, tag})
```
copy from [how to encrypt and decrypt with AES CBC 128 in Elixir](https://stackoverflow.com/questions/37629194/how-to-encrypt-and-decrypt-with-aes-cbc-128-in-elixir)
and something like below:

``` elixir
defmodule Encrypt do
  @aad "AES256GCM"
  ...
  def encrypt(val, key) do
    mode       = :aes_gcm
    secret_key = :base64.decode(key)
    iv         = :crypto.strong_rand_bytes(16)
    {ciphertext, ciphertag} = :crypto.block_encrypt(mode, secret_key, {@aad, to_string(val), 16})
  end
end
```
copy from [Building an Elixir Encryption Engine with Erlang's Crypto Module](https://www.thegreatcodeadventure.com/elixir-encryption-with-erlang-crypto/)

## use the new crypt api

```
crypto_one_time/4
crypto_one_time/5
crypto_one_time_aead/6
crypto_one_time_aead/7
```
example:

``` erlang
crypto:start().
Key = <<1:128>>.
IV = <<0:128>>.
Txt = [<<"First bytes">>,<<"Second bytes">>].
crypto:crypto_one_time(aes_128_ctr, Key, IV, Txt, true).

Txt2 = [<<"First bytes">>,<<"Second bytes">>].
AAD2 = <<"Some bytes">>.
crypto:crypto_one_time_aead(aes_128_gcm, Key, IV, Txt2, AAD2, true).
```
copy from [new_api](http://erlang.org/doc/apps/crypto/new_api.html)
