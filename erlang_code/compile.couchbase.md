# compile couchbase

the couchbase is all according to the web page
http://www.zewaren.net/site/?q=node/157

``` shell
mkdir couchbase
cd cochbase
repo init -u git://github.com/couchbase/manifest -m released/4.1.0.xml
repo sync

apt-get install cmake libsnappy-dev
unset GOBIN


# couchbase compile need erlang-src
export PATH=/usr/local/otp_src_19.2/bin:$PATH

# change the code according to the page,  all the erlang code warning should be fixed.
# all has done,  just type make
make
```
