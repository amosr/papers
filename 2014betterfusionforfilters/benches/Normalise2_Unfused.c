#include "inc/Common.h"

void normalise2(float* us, int us_len, float* out_nor1, float* out_nor2)
{
    // sum1 = fold (+) 0 us
    float sum1 = 0;
    for (int i = 0; i != us_len; ++i) {
        sum1 += us[i];
    }

    // gts = filter (>0) us
    float* gts = malloc(sizeof(float) * us_len);
    int gts_len = 0;
    for (int i = 0; i != us_len; ++i) {
        if (us[i] > 0) {
            gts[gts_len++] = us[i];
        }
    }

    // sum2 = fold (+) 0 gts
    float sum2 = 0;
    for (int i = 0; i != gts_len; ++i) {
        sum2 += gts[i];
    }
    free(gts);

    // nor1 = map (/sum1) us
    for (int i = 0; i != us_len; ++i) {
        out_nor1[i] = us[i] / sum1;
    }
    // nor2 = map (/sum2) us
    for (int i = 0; i != us_len; ++i) {
        out_nor2[i] = us[i] / sum2;
    }
}

int main(int argc, char** argv)
{
    int sz      = get_size(argc, argv);
    float* us   = random_floats(1, sz);
    float* nor1 = malloc(sizeof(float) * sz);
    float* nor2 = malloc(sizeof(float) * sz);
    normalise2(us, sz, nor1, nor2);
    printf("%f: %f %f\n", us[0], nor1[0], nor2[0]);
}
