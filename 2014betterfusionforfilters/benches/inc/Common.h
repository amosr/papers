#include <stdio.h>
#include <stdlib.h>

void usage(int argc, char** argv)
{
    printf("Usage: %s <size>", argv[0]);
    exit(1);
}

int get_size(int argc, char** argv)
{
    if (argc != 2) {
        usage(argc, argv);
    }

    int r = atoi(argv[1]);
    return r;
}


int* random_ints(int seed, int sz)
{
    int* buf = malloc(sizeof(int) * sz);
    for (int i = 0; i != sz; ++i) {
        buf[i] = rand();
    }

    return buf;
}

float* random_floats(int seed, int sz)
{
    float* buf = malloc(sizeof(float) * sz);
    for (int i = 0; i != sz; ++i) {
        buf[i] = rand() / 32768.0f;
    }

    return buf;
}
