#include <stdlib.h>


int dumb(int* A, int N)
{
    int n;
    int s1 = 0;
    int s2 = 0;

    for (n = 0; n != N; ++n) {
        s1 += A[n];
    }
    for (n = 0; n != N; ++n) {
        s2 -= A[n];
    }

    return s1 * s2;
}

