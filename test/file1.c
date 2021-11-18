#include <inttypes.h>
#include <wchar.h>
extern void *fd4_mkclosure(void*, int, ...);
extern void *fd4_printn(uint64_t);
extern void *fd4_sub(uint64_t, uint64_t);

void* fd4_a;
void* fd4_b;
uint64_t* fd4main() {
  fd4_a = (void *)(({
    fd4_sub( (uint64_t) (uint64_t) (uint64_t) 2 + (uint64_t) 3 + (uint64_t) 4
    , (uint64_t) ({
      void** fd4_x0 = 3;
      (uint64_t) fd4_x0 + (uint64_t) 1;
    }) );
  }));
  fd4_b = (void *)((uint64_t) fd4_a + (uint64_t) 5);
  fd4_printn((uint64_t)fd4_b)
  ;
  return 0;
}