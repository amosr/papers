#include <stdlib.h>


void normalise2(int* A, int N, int* nor1, int* nor2)
/* { let s1   = sum A
 *       gts  = filter (>0) A
 *       s2   = sum gts
 *       nor1 = map (/s1) A
 *       nor2 = map (/s2) A } */
{
    int n;

    // s1 = sum A
    int s1 = 0;
    for (n = 0; n != N; ++n) {
        s1 += A[n];
    }

    // gts = filter (>0) A
    int* gts = malloc(sizeof(int) * N);
    int gts_len = 0;
    for (n = 0; n != N; ++n) {
        if (A[n] > 0) {
            gts[gts_len++] = A[n];
        }
    }

    // s2 = sum gts
    int s2 = 0;
    for (n = 0; n != gts_len; ++n) {
        s2 += gts[n];
    }

    // nor1 = map (/s1) A
    for (n = 0; n != N; ++n) {
        nor1[n] = A[n] / s1;
    }

    // nor2 = map (/s2) A
    for (n = 0; n != N; ++n) {
        nor2[n] = A[n] / s2;
    }
}
