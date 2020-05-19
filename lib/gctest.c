#include "gc.h"
#include "unity/unity.h"
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

void setUp(void) {
    // set stuff up here
}

void tearDown(void) {
    // clean stuff up here
}

void test_mark_list(void) {
    unsigned heap_size = 12;
    int64_t *heap = calloc(heap_size, sizeof (int64_t));

    heap[0] = 0x00000004; // pair
    heap[1] = 0x00000000; // the number 0
    heap[2] = 0x7fffffff; // false

    heap[3] = 0x00000004; // pair
    heap[4] = 0x00000002; // the number 1
    heap[5] = (int64_t)(heap) + 1; // pointer to heap[0]

    heap[6] = 0x00000004; // pair
    heap[7] = 0x00000004; // the number 2
    heap[8] = (int64_t)(heap + 3) + 1; // pointer to heap[3]

    int64_t stack[5] = {
            ((int64_t)(heap + 6)) + 1, // a reference to the last pair on the heap
            0x0000000e, // the number 7
            0x00000000, // to be filled in with mock esp
            0x00000000, // to be filled in with mock return ptr
            0xffffffff
    };

    stack[2] = (int64_t)(stack + 4); // some address further "down"
    stack[3] = 0x401e05;       // mock return ptr/data to skip

    int64_t * expectHeap = calloc(heap_size, sizeof (int64_t));
    memcpy(expectHeap, heap, heap_size * (sizeof (int64_t)));
    // every pair should be marked
    expectHeap[0] = 0x0000000100000004;
    expectHeap[3] = 0x0000000100000004;
    expectHeap[6] = 0x0000000100000004;

    int64_t * max_address = mark(stack, (stack + 3), (stack + 4), heap);

    debug("heap: %p, max address: %p\n", heap, max_address);
    debug_print_heap(heap, heap_size);

    TEST_ASSERT_EQUAL_INT64_ARRAY(expectHeap, heap, heap_size);
    TEST_ASSERT_EQUAL(6, max_address - heap);

}

void test_gc(void) {
    unsigned heap_size = 12;
    int64_t * heap = calloc(heap_size, sizeof (int64_t));

    heap[0] = 0x0000000000000004; // a pair (that will be collected)
    heap[1] = ((int64_t)(heap + 3)) + 0x1; // another pair on the heap
    heap[2] = 0x00000030; // the number 24

    heap[3] = 0x0000000000000004; // a pair
    heap[4] = ((int64_t)(heap + 9)) + 0x1; // another pair on the heap
    heap[5] = 0x00000008; // the number 4

    heap[6] = 0x0000000000000004;
    heap[7] = 0x00000004; // the number 2
    heap[8] = 0x00000006; // the number 3

    heap[9]  = 0x0000000000000004;
    heap[10] = 0x0000000a; // the number 5
    heap[11] = 0x0000000c; // the number 6

    int64_t stack[5] = {
            ((int64_t)(heap + 3)) + 1, // a reference to the second pair on the heap
            0x0000000e, // the number 7
            0x00000000, // to be filled in with mock esp
            0x00000000, // to be filled in with mock return ptr
            0xffffffff
    };

    stack[2] = (int64_t)(stack + 4); // some address further "down"
    stack[3] = 0x0adadad0;       // mock return ptr/data to skip

    // MARK
    int64_t * expectHeap = calloc(heap_size, sizeof (int64_t));
    memcpy(expectHeap, heap, heap_size * (sizeof (int64_t)));
    expectHeap[3] = 0x0000000100000004;
    expectHeap[9] = 0x0000000100000004;

    int64_t * max_address = mark(stack, (stack + 3), (stack + 4), heap);

    TEST_ASSERT_EQUAL_INT64_ARRAY(expectHeap, heap, heap_size);
    TEST_ASSERT_EQUAL(9, max_address - heap);

    debug("heap: %p, max address: %p\n", heap, max_address);
    debug_print_heap(heap, heap_size);
    debug("#############################\n");
    debug_print_heap(expectHeap, heap_size);

    // FORWARD

    int64_t * expectHeap2 = calloc(heap_size, sizeof (int64_t));
    memcpy(expectHeap2, heap, heap_size * (sizeof (int64_t)));

    forward(stack, (stack + 3), stack + 4, heap, max_address);

    // the tuple beginning at 3 should now be forwarding to 0
    expectHeap2[3] = (((int64_t)heap) << 8) + 4;
    // the pointer at 4 should be updated to the new address
    expectHeap2[4] = ((int64_t)(heap + 3)) + 1;
    expectHeap2[9] = (((int64_t)(heap + 3)) << 8) + 4;

    debug_print_heap(heap, heap_size);
    debug("#############################\n");
    debug_print_heap(expectHeap2, heap_size);

    TEST_ASSERT_EQUAL_INT64_ARRAY(expectHeap2, heap, heap_size);

    // COMPACT

    int64_t * expectHeap3 = calloc(heap_size, sizeof (int64_t));

    int64_t *last_addr = compact(heap, max_address, heap + heap_size);

    TEST_ASSERT_EQUAL(6, last_addr - heap);

    expectHeap3[0]  = 0x00000004; // a pair
    expectHeap3[1]  = ((int64_t)(heap + 3)) | 0x00000001; // another pair on the heap
    expectHeap3[2]  = 0x00000008; // the number 4

    expectHeap3[3]  = 0x00000004;
    expectHeap3[4]  = 0x0000000a; // the number 5
    expectHeap3[5]  = 0x0000000c; // the number 6


    debug_print_heap(heap, heap_size);
    debug("#############################\n");
    debug_print_heap(expectHeap3, heap_size);

    TEST_ASSERT_EQUAL_INT64_ARRAY(expectHeap3, heap, 6);

    free(heap);
    free(expectHeap);
    free(expectHeap2);
    free(expectHeap3);
}



int main(void) {
    UNITY_BEGIN();
    RUN_TEST(test_mark_list);
    RUN_TEST(test_gc);

    return UNITY_END();
}