#include <stddef.h>
#include <stdint.h>


// comment the following line to disable debugging prints
// #define ENABLE_DEBUG

int debug(const char *format, ...);
void debug_print_heap(int64_t* heap, int64_t size);

/*
  Mark, sweep, and compact memory.
  Arguments:
  
    - stack_bottom: A pointer to the bottom of the stack (highest address). In
                    our runtime, this is set by getting the address of a
                    variable from main
    - current_ESP: current RSP
    - stack_top: A pointer to the top of the stack (lowest address). It points
                 to the last stack frame's last variable.
    - heap_start: A pointer to the start of the heap; typically the global value
                  HEAP, but provided as a parameter for testing
  
    - heap_end: A pointer to the end of the heap (exclusive)
  Returns:
    The address immediately following the compacted data, to use as the new
    allocation index stored in EBX
*/
int64_t* gc(int64_t* stack_bottom, int64_t* current_RSP, int64_t* stack_top, int64_t* heap_start, int64_t* heap_end);



/*
  mark()
  Traverse the heap, starting from the data on the stack, and mark all
  reachable pairs as live, by setting the first word to 0x00000001
  Arguments:
    - stack_top, current_RSP, stack_bottom, heap_start: as in gc()
  Returns:
    The address of the marked pair with the highest address. By starting
    address, this means the word holding the GC word.
*/
int64_t* mark(int64_t* stack_top, int64_t* current_RSP, int64_t* stack_bottom, int64_t* heap_start);



/*
  forward()
  Set up forwarding pointers to marked data on the heap:
  
    1. Set the first word of each live pair on the heap to its new, compacted
       address (with a 1 at the end, to track liveness)
    2. Set each address value on the stack and on the heap to point at the new
       corresponding forwarded value.
  Arguments:
    
    - stack_top, current_RSP, stack_bottom, heap_start: As in gc()
    - max_address: the output of function mark()
  Returns:
    nothing
*/
void forward(int64_t* stack_top, int64_t* current_RSP, int64_t* stack_bottom, int64_t* heap_start, int64_t* max_address);

/*
  compact()
  Compact memory, assuming that the data has been marked and set up for
  forwarding. Copy the memory for each value onto the heap into its new
  location.
  Arguments:
  
    - heap_start: A pointer to the start of the heap; typically the global value
      HEAP, but provided as a parameter for testing
    - max_address: A pointer to the _start_ of the latest pair marked (e.g. the
      return value of the mark call), for knowing how far in the heap to
      traverse
  Returns:
    The address immediately following the compacted data, to use as the new
    starting allocation index for EBX
*/
int64_t* compact(int64_t* heap_start, int64_t* max_address, int64_t* heap_end);