#include "inc/Points.h"

float closestpoints(Points ps)
{
    if (ps.size < 100) {
        return naive(ps);
    }

    float y = select_y(ps);

    // aboves = filter (above y) pts
    // belows = filter (below y) pts
    Points aboves = alloc_points(ps.size);
    int aboves_size = 0;
    Points belows = alloc_points(ps.size);
    int belows_size = 0;
    for (int i = 0; i != ps.size; ++i) {
        if (ps.ys[i] < y) {
            aboves.xs[aboves_size] = ps.xs[i];
            aboves.ys[aboves_size] = ps.ys[i];
            aboves_size++;
        }
        if (ps.ys[i] >= y) {
            belows.xs[belows_size] = ps.xs[i];
            belows.ys[belows_size] = ps.ys[i];
            belows_size++;
        }
    }
    aboves.size = aboves_size;
    belows.size = belows_size;

    float above_ = closestpoints(aboves);
    float below_ = closestpoints(belows);

    free_points(aboves);
    free_points(belows);

    float border = minf(above_, below_);


    // aboveB = filter (above y && below (y-border)) pts
    // belowB = filter (below y && above (y+border)) pts
    Points aboveB = alloc_points(ps.size);
    int aboveB_size = 0;
    Points belowB = alloc_points(ps.size);
    int belowB_size = 0;
    for (int i = 0; i != ps.size; ++i) {
        if (ps.ys[i] < y && ps.ys[i] >= (y-border)) {
            aboveB.xs[aboveB_size] = ps.xs[i];
            aboveB.ys[aboveB_size] = ps.ys[i];
            aboveB_size++;
        }
        if (ps.ys[i] >= y && ps.ys[i] < (y+border)) {
            belowB.xs[belowB_size] = ps.xs[i];
            belowB.ys[belowB_size] = ps.ys[i];
            belowB_size++;
        }
    }
    aboveB.size = aboveB_size;
    belowB.size = belowB_size;

    //  dists   = V.concatMap (\p -> V.map (dist p) belowB) aboveB
    float min = border;
    for (int i = 0; i != aboveB.size; ++i) {
        for (int j = 0; j != belowB.size; ++j) {
            float d
                = dist( aboveB.xs[i], aboveB.ys[i]
                      , belowB.xs[j], belowB.ys[j] );
            min = minf(min, d);
        }
    }

    free_points(aboveB);
    free_points(belowB);

    return min;
}

int main(int argc, char** argv)
{
    int sz      = get_size(argc, argv);
    float* xs   = random_floats(1, sz);
    float* ys   = random_floats(2, sz);
    Points ps;
    ps.size = sz;
    ps.xs = xs;
    ps.ys = ys;

    float cp = closestpoints(ps);
    printf("(%f,%f): %f\n", xs[0], ys[1], cp);
}
