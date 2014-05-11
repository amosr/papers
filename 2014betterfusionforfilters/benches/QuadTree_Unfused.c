#include "inc/Quad.h"

Box bounds(Points ps)
{
    float x1 = ps.xs[0];
    for (int i = 0; i != ps.size; ++i) {
        x1 = minf(x1, ps.xs[i]);
    }

    float x2 = ps.xs[0];
    for (int i = 0; i != ps.size; ++i) {
        x2 = maxf(x2, ps.xs[i]);
    }

    float y1 = ps.ys[0];
    for (int i = 0; i != ps.size; ++i) {
        y1 = minf(y1, ps.ys[i]);
    }

    float y2 = ps.ys[0];
    for (int i = 0; i != ps.size; ++i) {
        y2 = maxf(y2, ps.ys[i]);
    }

    Box b;
    b.tlx = x1;
    b.tly = y1;
    b.brx = x2 + 1;
    b.bry = y2 + 1;

    return b;
}

Tree* quadtree_go(Box b, Points ps)
{
    if (ps.size == 0) {
        return mkNil();
    }
    if (ps.size == 1) {
        return mkLeaf(ps.xs[0], ps.ys[0]);
    }

    Box bs[4];
    splitbox(b, bs);

    Points p0 = alloc_points(ps.size);
    p0.size = 0;
    for (int i = 0; i != ps.size; ++i) {
        if (inbox(bs[0], ps.xs[i], ps.ys[i])) {
            p0.xs[p0.size] = ps.xs[i];
            p0.ys[p0.size] = ps.ys[i];
            p0.size++;
        }
    }

    Points p1 = alloc_points(ps.size);
    p1.size = 0;
    for (int i = 0; i != ps.size; ++i) {
        if (inbox(bs[1], ps.xs[i], ps.ys[i])) {
            p1.xs[p1.size] = ps.xs[i];
            p1.ys[p1.size] = ps.ys[i];
            p1.size++;
        }
    }

    Points p2 = alloc_points(ps.size);
    p2.size = 0;
    for (int i = 0; i != ps.size; ++i) {
        if (inbox(bs[2], ps.xs[i], ps.ys[i])) {
            p2.xs[p2.size] = ps.xs[i];
            p2.ys[p2.size] = ps.ys[i];
            p2.size++;
        }
    }

    Points p3 = alloc_points(ps.size);
    p3.size = 0;
    for (int i = 0; i != ps.size; ++i) {
        if (inbox(bs[3], ps.xs[i], ps.ys[i])) {
            p3.xs[p3.size] = ps.xs[i];
            p3.ys[p3.size] = ps.ys[i];
            p3.size++;
        }
    }


    Tree* t0 = quadtree_go(bs[0], p0);
    Tree* t1 = quadtree_go(bs[1], p1);
    Tree* t2 = quadtree_go(bs[2], p2);
    Tree* t3 = quadtree_go(bs[3], p3);

    free_points(p0);
    free_points(p1);
    free_points(p2);
    free_points(p3);


    return mkTree(t0, t1, t2, t3);
}

Tree* quadtree(Points ps)
{
    Box b = bounds(ps);
    return quadtree_go(b, ps);
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

    Tree* tree = quadtree(ps);
    int size = tree_size(tree);
    printf("%d\n", size);
}
