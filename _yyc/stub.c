#include <stdio.h>

void su_stub(int line) {
    printf("(%d)\n", line);
    fflush(stdout);
}