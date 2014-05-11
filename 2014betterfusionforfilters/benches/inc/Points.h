#include "Common.h"
#include <math.h>

typedef struct Points_s {
    int  size;
    float* xs;
    float* ys;
} Points;

float PointsEqual = 100000000000;

Points alloc_points(int size) {
    Points p;
    p.size = size;
    p.xs = malloc(sizeof(float) * size);
    p.ys = malloc(sizeof(float) * size);
    return p;
}

void free_points(Points p) {
    free(p.xs);
    free(p.ys);
}

float dist(float x1, float y1, float x2, float y2)
{
    if (x1 == x2 && y1 == y2)
        return PointsEqual;
    float x = x1 - x2;
    float y = y1 - y2;
    return sqrt(x * x + y * y);
}

float naive(Points ps)
{
    float min = PointsEqual;
    for (int i = 0; i != ps.size; ++i) {
        for (int j = 0; j != ps.size; ++j) {
            if (i != j) {
                float d = dist( ps.xs[i], ps.ys[i]
                              , ps.xs[j], ps.ys[j]);
                if (d < min)
                    min = d;
            }
        }
    }

    return min;
}

float select_y(Points ps)
{
    double sum = 0;
    for (int i = 0; i != ps.size; ++i) {
        sum += ps.ys[i];
    }

    return sum / ps.size;
}

float minf(float a, float b) {
    if (a < b) {
        return a;
    } else {
        return b;
    }
}

float maxf(float a, float b) {
    if (a > b) {
        return a;
    } else {
        return b;
    }
}

