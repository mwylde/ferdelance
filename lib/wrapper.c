#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <inttypes.h>
#include <string.h>
#include <limits.h>
#include <errno.h>
#include "gc.h"

extern int64_t our_code_starts_here() asm("our_code_starts_here");
extern int64_t print(int64_t val) asm("print");
extern void error(int64_t val) asm("error");
extern int64_t input(int64_t i)  asm("input");

extern int64_t* HEAP_END     asm("HEAP_END");
extern int64_t* STACK_BOTTOM asm("STACK_BOTTOM");
extern int64_t* try_gc(int64_t* alloc_ptr, int64_t amount_needed, int64_t* current_RSP, int64_t* stack_top) asm("try_gc");


const int64_t TRUE  = 0xFFFFFFFF;
const int64_t FALSE = 0x7FFFFFFF;
const int64_t FDL_INT_MIN = -(1LL << 62);
const int64_t FDL_INT_MAX = (1LL << 62) - 1;

int64_t INPUT_COUNT = 0;
int64_t* INPUTS = NULL;


// -------------------------------------------
// Global variables used in garbage collection
// -------------------------------------------
size_t HEAP_SIZE;
int64_t* STACK_BOTTOM;
int64_t* HEAP;
int64_t* HEAP_END;
// -------------------------------------------


void error(int64_t error_code) {
    if(error_code == 1) {
        fprintf(stderr, "expected a number\n");
        exit(1);
    } else if (error_code == 2) {
        fprintf(stderr, "expected a bool\n");
        exit(2);
    } else if (error_code == 3) {
        fprintf(stderr, "overflow\n");
        exit(3);
    } else if (error_code == 4) {
        fprintf(stderr, "expected a tuple\n");
        exit(4);
    } else if (error_code == 5) {
        fprintf(stderr, "index too small\n");
        exit(5);
    } else if (error_code == 6) {
        fprintf(stderr, "index too large\n");
        exit(6);
    } else if (error_code == 7) {
        fprintf(stderr, "expected a lambda\n");
        exit(7);
    } else {
        fprintf(stderr, "unknown error: %" PRId64 "\n", error_code);
        exit(-1);
    }
}

int64_t print_inner(int64_t val) {
    if ((val & 3) == 3) {
        if (val == TRUE) {
            printf("true");
            return val;
        } else if (val == FALSE) {
            printf("false");
            return val;
        }
    } else if ((val & 5) == 1) {
        int64_t *ptr = (int64_t *) (val - 1);
        int64_t len = *ptr / 2;
        ptr++;
        printf("(");
        for (int64_t i = 0; i < len; i++) {
            print_inner(*ptr);
            if (i != len - 1) {
                printf(", ");
            }
            ptr++;
        }
        printf(")");

        return val;
    } else if ((val & 5) == 5) {
        printf("(λ)");
        return val;
    } else {
        printf("%" PRId64, val / 2);
        return val;
    }

    printf("Unknown value: %" PRIx64, val);
    return val;
}

int64_t print(int64_t val) {
    print_inner(val);
    printf("\n");
    return val;
}

int64_t input(int64_t i) {
    i = i >> 1;

    if (i < 0 || i >= INPUT_COUNT) {
        fprintf(stderr, "input index out of bounds (given:%d #args:%d) \n", (int) i, (int) INPUT_COUNT);
        exit(1);
    }

    return INPUTS[(int) i];
}

int64_t parse_input(const char* in) {
    if (strcmp(in, "true") == 0) {
        return TRUE;

    } else if (strcmp(in, "false") == 0) {
        return FALSE;

    } else {
        size_t l = strlen(in);
        if (l == 0) {
            fprintf(stderr, "input is empty\n");
            exit(1);
        }

        char* endptr = (char*) &in[l];
        long int r = strtol(in, &endptr, 10);

        if (*endptr != '\0') {
            fprintf(stderr, "input '%s' is not a number or a boolean\n", in);
            exit(1);
        }

        if (r < FDL_INT_MIN || r > FDL_INT_MAX) {
            fprintf(stderr, "input '%s' is not a representable number\n", in);
            exit(1);
        }

        return (int64_t) r * 2;
    }
}

int64_t* try_gc(int64_t* alloc_ptr, int64_t bytes_needed, int64_t* current_ESP, int64_t* stack_top) {
    if(HEAP == alloc_ptr) {
        fprintf(stderr,
                "out of memory: Allocation of %" PRId64 " words too large for %" PRId64 "-word heap\n",
                bytes_needed / 8,
                (int64_t) HEAP_SIZE);
        exit(1);
    }

    int64_t* new_ebx = gc(STACK_BOTTOM, current_ESP, stack_top, HEAP, HEAP_END);

    if((new_ebx + (bytes_needed / 8)) > HEAP_END) {
        fprintf(stderr,
                "out of memory: Needed %" PRId64 " words, but only %" PRId64 " remain after collection",
                bytes_needed / 8,
                HEAP_END - new_ebx);
        exit(1);
    }

    return new_ebx;
}


int main(int argc, char** argv) {
    const char* heap_size_env = getenv("HEAP_SIZE");
    if(heap_size_env != NULL) {
        char *endptr;
        HEAP_SIZE = strtol(heap_size_env, &endptr, 10);

        if ((errno == ERANGE && (heap_size_env == LONG_MAX || heap_size_env == LONG_MIN))
            || (errno != 0 && heap_size_env == 0) || endptr == heap_size_env) {
            fprintf(stderr, "Invalid heap size");
            return 3;
        }
    } else {
        HEAP_SIZE = 100000;
    }

    INPUT_COUNT = argc > 1 ? argc - 1 : 0;

    if (INPUT_COUNT > 0) {
        INPUTS = calloc(INPUT_COUNT, sizeof(int));

        int i = 0;
        for (; i < argc - 1; i++) {
            INPUTS[i] = parse_input(argv[i+1]);
        }
    }

    HEAP = calloc(HEAP_SIZE, sizeof (int64_t));

    if (HEAP == NULL) {
        fprintf(stderr, "HEAP is null\n");
        exit(1);
    } else if (((uint64_t) HEAP) & 0x3) {
        fprintf(stderr, "last 2 bits of HEAP is not 0 !\n");
        exit(1);
    }

    HEAP_END = HEAP + HEAP_SIZE;

    int64_t result = our_code_starts_here(HEAP);

    print(result);

    return 0;
}
