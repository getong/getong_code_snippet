# erlang maps

## from_list

``` c
/* maps:from_list/1
 * List may be unsorted [{K,V}]
 */

BIF_RETTYPE maps_from_list_1(BIF_ALIST_1) {
    Eterm item = BIF_ARG_1, res, *kv;
    Uint  size = 0;
    if (is_list(item) || is_nil(item)) {

	/* Calculate size and check validity */

	while(is_list(item)) {
	    res = CAR(list_val(item));
	    if (is_not_tuple(res))
		goto error;

	    kv = tuple_val(res);
	    if (*kv != make_arityval(2))
		goto error;

	    size++;
	    item = CDR(list_val(item));
	}

	if (is_not_nil(item))
	    goto error;

	if (size > MAP_SMALL_MAP_LIMIT) {
	    BIF_RET(hashmap_from_validated_list(BIF_P, BIF_ARG_1, size));
	} else {
	    BIF_RET(flatmap_from_validated_list(BIF_P, BIF_ARG_1, size));
	}
    }

error:

    BIF_ERROR(BIF_P, BADARG);
}

static Eterm flatmap_from_validated_list(Process *p, Eterm list, Uint size) {
    Eterm *kv, item = list;
    Eterm *hp, *thp,*vs, *ks, keys, res;
    flatmap_t *mp;
    Uint  unused_size = 0;
    Sint  c = 0;
    Sint  idx = 0;


    hp    = HAlloc(p, 3 + 1 + (2 * size));
    thp   = hp;
    keys  = make_tuple(hp);
    *hp++ = make_arityval(size);
    ks    = hp;
    hp   += size;
    mp    = (flatmap_t*)hp;
    res   = make_flatmap(mp);
    hp   += MAP_HEADER_FLATMAP_SZ;
    vs    = hp;

    mp->thing_word = MAP_HEADER_FLATMAP;
    mp->size = size; /* set later, might shrink*/
    mp->keys = keys;

    if (size == 0)
	return res;

    /* first entry */
    kv    = tuple_val(CAR(list_val(item)));
    ks[0] = kv[1];
    vs[0] = kv[2];
    size  = 1;
    item  = CDR(list_val(item));

    /* insert sort key/value pairs */
    while(is_list(item)) {

	kv = tuple_val(CAR(list_val(item)));

	/* compare ks backwards
	 * idx represent word index to be written (hole position).
	 * We cannot copy the elements when searching since we might
	 * have an equal key. So we search for just the index first =(
	 *
	 * It is perhaps faster to move the values in the first pass.
	 * Check for uniqueness during insert phase and then have a
	 * second phace compacting the map if duplicates are found
	 * during insert. .. or do someother sort .. shell-sort perhaps.
	 */

	idx = size;

	while(idx > 0 && (c = CMP_TERM(kv[1],ks[idx-1])) < 0) { idx--; }

	if (c == 0) {
	    /* last compare was equal,
	     * i.e. we have to release memory
	     * and overwrite that key/value
	     */
	    ks[idx-1] = kv[1];
	    vs[idx-1] = kv[2];
	    unused_size++;
	} else {
	    Uint i = size;
	    while(i > idx) {
		ks[i] = ks[i-1];
		vs[i] = vs[i-1];
		i--;
	    }
	    ks[idx] = kv[1];
	    vs[idx] = kv[2];
	    size++;
	}
	item = CDR(list_val(item));
    }

    if (unused_size) {
	/* the key tuple is embedded in the heap
	 * write a bignum to clear it.
	 */
	/* release values as normal since they are on the top of the heap */

	ks[size] = make_pos_bignum_header(unused_size - 1);
	HRelease(p, vs + size + unused_size, vs + size);
    }

    *thp = make_arityval(size);
    mp->size = size;
    return res;
}
```
copy from erts/emulator/beam/erl_map.c
