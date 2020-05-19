#include <stdio.h>
#include <stdarg.h>
#include <inttypes.h>
#include <stdbool.h>
#include "gc.h"

int64_t get_length(const int64_t *cur);

int debug(const char *format, ...) {
#ifdef ENABLE_DEBUG
    va_list args;
    va_start(args, format);

    int rc = vfprintf(stderr, format, args);
    fflush(stderr);
    return rc;
#else
    return 0;
#endif
}

void debug_print_heap(int64_t * heap, int64_t size) {
    for(int i = 0; i < size; i += 1) {
        debug("  %3d/%p: %-10p (%d)\n", i, (heap + i), (int64_t*)(*(heap + i)), *(heap + i));
    }
}

uint64_t* max(uint64_t* a, uint64_t* b) {
    return a > b ? a : b;
}

uint64_t* is_ptr(const uint64_t* val) {
    if ((*val & 3u) == 1) {
        uint64_t* addr = (uint64_t *)(*val & 0xFFFFFFFFFFFFFFF0);
        // debug("found pointer: %p -> %" PRId64 "\n", addr, *addr);1
        return addr;
    }
    return NULL;
}

uint64_t* mark_heap_value(int64_t* cur, uint64_t* max_addr) {
    uint64_t* addr = is_ptr((uint64_t *) cur);
    if (addr == NULL) {
        // this isn't a pointer, skip
        return max_addr;
    }

    // stop, if this already marked
    if ((*addr >> 32ull) == 1) {
        return max(addr, max_addr);
    }

    debug("\tMarking %p\n", addr);

    uint64_t length = *addr / 2;
    if (length % 2 == 0) length++;

    // otherwise, mark it
    (*addr) |= (1ull << 32ull);

    max_addr = max(addr, max_addr);

    // skip header
    addr++;

    // iterate through its elements, and if we find a pointer, DFS it
    for (uint64_t i = 0; i < length; i++) {
        max_addr = mark_heap_value((int64_t *)(addr + i), max_addr);
    }

    return max_addr;
}

void* stack_traverse(int64_t* stack_top, int64_t* current_RSP, int64_t* stack_bottom,
                     void* ctx,
                     void* (*process)(int64_t*, void* ctx)) {
    int64_t* cur = stack_top;
    bool past_cur_frame = false;
    int64_t* old_rsp = NULL;
    // skip the top 3 words: the main() return pointer, arg1 to our_code, and the heap start address
    while (cur <= stack_bottom - 3) {
        if (!past_cur_frame) {
            // at the top of stack will be our current (partial) frame, which ends once we get to RSP
            debug("--CURRENT FRAME START--\n");

            while (true) {
                if (cur == current_RSP) {
                    debug("--CURRENT FRAME END--\n\n");
                    past_cur_frame = true;
                    old_rsp = (int64_t *)*(cur-1);
                    cur++;
                    break;
                }

                debug("%p 0x%" PRIx64 " \n", cur, *cur);

                ctx = process(cur, ctx);
                cur++;
            }
        }

        // we're past our current frame, now we're working with full frames
        debug("--BEGIN FRAME--\n");
        while (cur <= stack_bottom - 3) {
            if (cur == old_rsp - 1) {
                old_rsp = (int64_t *) *cur;
                // skip over old_rsp and return pointer
                cur += 2;
                break;
            }

            debug("%p 0x%" PRIx64 " \n", cur, *cur);
            ctx = process(cur, ctx);
            cur++;
        }
        debug("--END FRAME--\n\n");
    }

    return ctx;
}

int64_t* mark(int64_t* stack_top, int64_t* current_RSP, int64_t* stack_bottom, int64_t* heap_start) {
    debug("--------------------------------------------------\n");
    debug("1. MARK\n");
    debug("--------------------------------------------------\n");

    // iterate through the stack
    debug("stack bottom: %p; stack top: %p; diff: %d; esp: %p\n", stack_bottom, stack_top, stack_bottom - stack_top, current_RSP);

    // max_addr = mark_heap_value((uint64_t *)cur, max_addr);

    return (int64_t *) stack_traverse(stack_top, current_RSP, stack_bottom, NULL,
                                      (void *(*)(int64_t *, void *)) &mark_heap_value);;
}

uint64_t tag(int64_t* old, int64_t* new) {
    uint64_t tag = 1;
    if (((uint64_t) *old & 5) == 5) {
        tag = 5;
    }

    return (uint64_t) new + tag;
}

void* forward_stack(int64_t* cur, void* ctx) {
    uint64_t *addr = is_ptr((uint64_t *) cur);
    if (addr != NULL) {
        uint64_t new_addr = tag(cur, (int64_t *) (*addr >> 8ull));
        debug("updating stack pointer %p -> %" PRIx64 "\n", addr, new_addr);
        *cur = new_addr;
    }

    return NULL;
}


void forward(int64_t* stack_top, int64_t* current_RSP, int64_t* stack_bottom, int64_t* heap_start, int64_t* max_address) {
    debug("--------------------------------------------------\n");
    debug("2. FORWARD\n");
    debug("--------------------------------------------------\n");

    debug("heap start: %p\n", heap_start);
    int64_t* cur = heap_start;
    int64_t* next_space = heap_start;
    // calculate forwarding addresses for everything
    while (cur <= max_address) {
        int64_t length = (*cur & 0xFF) / 2;
        if (length % 2 == 0) length++;
        debug("inspecting heap %p with length %d\n", cur, length);

        if (((*cur & (1ull << 32ull)) != 0)) {
            // it's marked
            *cur = *cur & 0xFF;
            *cur += (((uint64_t) next_space) << 8);
            debug("forwarded %p -> 0x%" PRIx64 "\n", cur, next_space);
            next_space += length + 1;
        }
        cur += length + 1;
    }

    // update addresses in the heap
    cur = heap_start;
    while (cur <= max_address) {
        int64_t length = get_length(cur);

        if ((*cur >> 8) == 0) {
            // if it's not marked, skip updating it
            debug("skipping heap update: %p %" PRIx64 "\n", cur, *cur);
            cur += length + 1;
            continue;
        }
        debug("checking heap update: %p %" PRIx64 "\n", cur, *cur);
        // skip header
        cur++;

        // iterate through all of the values, updating addresses we find
        for (int i = 0; i < length; i++) {
            uint64_t *addr = is_ptr((uint64_t*) cur);
            if (addr != NULL) {
                uint64_t new_addr = tag(cur, (int64_t *) (*addr >> 8ull));
                debug("  updating heap pointer %p -> %" PRIx64 "\n", addr, new_addr);
                *cur = new_addr;
            }
            cur++;
        }
    }

    // update addresses in the stack
    stack_traverse(stack_top, current_RSP, stack_bottom, NULL, forward_stack);
}

int64_t* compact(int64_t* heap_start, int64_t* max_address, int64_t* heap_end) {
    debug("--------------------------------------------------\n");
    debug("3. COMPACT\n");
    debug("--------------------------------------------------\n");

    int64_t* last_addr = heap_start;
    int64_t* cur = heap_start;
    while (cur <= max_address) {
        int64_t length = get_length(cur);

        debug("compacting %p %" PRIx64 " (%d)\n", cur, *cur, length);

        // if it's not marked, skip it
        if ((*cur >> 8) == 0) {
            // if it's not marked, skip it
            cur += length + 1;
            continue;
        }

        int64_t* new_addr = (int64_t *)(*cur >> 8ull);

        // write the length back
        *new_addr = *cur & 0xFF;;

        debug("moving %p -> %p (%d)\n", cur, new_addr, *new_addr);

        cur++;
        new_addr++;
        for (int64_t i = 0; i < length; i++) {
            // copy the values
            *(new_addr + i) = *(cur + i);
        }

        last_addr = (int64_t*) new_addr + length;
        cur += length;
    }

    while (cur <= heap_end) {
        *cur = 0xcab005e;
        cur++;
    }

    return last_addr;
}

int64_t get_length(const int64_t *cur) {
    int64_t length = (*cur & 0xFF) / 2;
    if (length % 2 == 0) {
        length++;
    }
    return length;
}

int64_t* gc(int64_t* stack_bottom, int64_t* current_RSP, int64_t* stack_top, int64_t* heap_start, int64_t* heap_end) {
    debug("\n##################################################\n");
    debug("GC\n");
    debug("##################################################\n");

    debug_print_heap(heap_start, heap_end - heap_start);

    int64_t* max_address = mark(stack_top, current_RSP, stack_bottom, heap_start);
    debug("\n");
    debug_print_heap(heap_start, heap_end - heap_start);
    debug("\n");
    forward(stack_top, current_RSP, stack_bottom, heap_start, max_address);

    debug_print_heap(heap_start, heap_end - heap_start);

    debug("\n");
    int64_t* answer = compact(heap_start, max_address, heap_end);
    debug_print_heap(heap_start, heap_end - heap_start);

    return answer;
}
