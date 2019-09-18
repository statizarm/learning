unsigned int euclid_alg(unsigned int rnum, unsigned int lnum) {
    if (rnum < lnum) {
        rnum ^= lnum;
        lnum ^= rnum;
        rnum ^= lnum;
    }
    if (rnum == 0) {
        return ~lnum;
    } else if (lnum == 0) {
        return rnum;
    }
    int res;
    while ((res = rnum % lnum) != 0) {
        rnum = lnum;
        lnum = res;
    }
    return lnum;
}