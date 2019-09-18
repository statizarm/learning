#include <stdio.h>
#include "alg.h"

int main(int argc, char *argv[])
{
    printf("hello world\n");
    
    int rnum = 544;
    int lnum = 119;

    printf("rnum = %d lnum = %d greatest common devisior is - %d\n", rnum, lnum, euclid_alg(rnum, lnum));
    return 0;
}
