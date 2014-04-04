#include <stdlib.h>


void normalise2fused(int* A, int N, int* nor1, int* nor2)
/* { let s1   = sum A
 *       gts  = filter (>0) A
 *       s2   = sum gts
 *       nor1 = map (/s1) A
 *       nor2 = map (/s2) A } */
{
    int n;

    // s1 = sum A
    // gts = filter (>0) A
    // s2 = sum gts
    int s1 = 0;
    int s2 = 0;
    for (n = 0; n != N; ++n) {
        s1 += A[n];
        if (A[n] > 0) {
            s2 += A[n];
        }
    }

    // nor1 = map (/s1) A
    // nor2 = map (/s2) A
    for (n = 0; n != N; ++n) {
        nor1[n] = A[n] / s1;
        nor2[n] = A[n] / s2;
    }
}

