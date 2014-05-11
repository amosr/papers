#include "Points.h"

typedef
enum Tag_e { TLeaf, TNil, TTree }
Tag;

struct Tree_s;
typedef union Tree_data_u {
    // Leaf:
    float pos[2];
    // Tree:
    struct Tree_s* tree[4];
} Tree_data;

typedef struct Tree_s {
    Tag tag;
    Tree_data val;

} Tree;

Tree* mkLeaf(float x, float y) {
    Tree* r = malloc(sizeof(Tree));
    r->tag = TLeaf;
    r->val.pos[0] = x;
    r->val.pos[1] = y;
    return r;
}

Tree* mkNil() {
    Tree* r = malloc(sizeof(Tree));
    r->tag = TNil;
    return r;
}

Tree* mkTree(Tree* a, Tree* b, Tree* c, Tree* d) {
    Tree* r = malloc(sizeof(Tree));
    r->tag = TTree;
    r->val.tree[0] = a;
    r->val.tree[1] = b;
    r->val.tree[2] = c;
    r->val.tree[3] = d;
    return r;
}


typedef struct Box_s {
    float tlx, tly, brx, bry;
} Box;

int inbox(Box b, float x, float y) {
    return  x >= b.tlx
         && x <  b.brx
         && y >= b.tly
         && y <  b.bry;
}

void splitbox(Box b, Box* outs) {
    float mx = (b.tlx + b.brx) / 2;
    float my = (b.tly + b.bry) / 2;

    outs[0].tlx = b.tlx;
    outs[0].tly = b.tly;
    outs[0].brx =    mx;
    outs[0].bry =    my;

    outs[1].tlx =    mx;
    outs[1].tly = b.tly;
    outs[1].brx = b.brx;
    outs[1].bry =    my;

    outs[2].tlx = b.tlx;
    outs[2].tly =    my;
    outs[2].brx =    mx;
    outs[2].bry = b.bry;

    outs[3].tlx =    mx;
    outs[3].tly =    my;
    outs[3].brx = b.brx;
    outs[3].bry = b.bry;
}


int tree_size(Tree* t) {
    switch (t->tag) {
        case TLeaf:
            return 1;
        case TNil:
            return 0;
        case TTree:
            return tree_size(t->val.tree[0])
                 + tree_size(t->val.tree[1])
                 + tree_size(t->val.tree[2])
                 + tree_size(t->val.tree[3]);
    }
}
