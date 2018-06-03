# erlang zotonic

## base64url

b64e
``` erlang

%% accessors
b64e(X) ->
    element(X + 1,
        {$A, $B, $C, $D, $E, $F, $G, $H, $I, $J, $K, $L, $M, $N,
            $O, $P, $Q, $R, $S, $T, $U, $V, $W, $X, $Y, $Z,
            $a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m, $n,
            $o, $p, $q, $r, $s, $t, $u, $v, $w, $x, $y, $z,
            $0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $-, $_}).
```
b64d

``` erlang
b64d(X) ->
    b64d_ok(element(X, ?DECODE_MAP)).

b64d(X) ->
    b64d_ok(element(X, ?DECODE_MAP)).

    %% One-based decode map.
-define(DECODE_MAP,
    {bad, bad, bad, bad, bad, bad, bad, bad, ws, ws, bad, bad, ws, bad, bad, %1-15
        bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, %16-31
        ws, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, 62, bad, bad, %32-47
        52, 53, 54, 55, 56, 57, 58, 59, 60, 61, bad, bad, bad, eq, bad, bad, %48-63
        bad, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, %64-79
        15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, bad, bad, bad, bad, 63, % 80-95
        bad, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
        41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, bad, bad, bad, bad, bad,
        bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad,
        bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad,
        bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad,
        bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad,
        bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad,
        bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad,
        bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad,
        bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad}).
```
why the `DECOCDE_MAP` elements like this, do not know.
