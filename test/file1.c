#include <inttypes.h>
#include <wchar.h>
extern void *fd4_mkclosure(void*, int, ...);
extern void *fd4_printn(uint64_t);
extern void *fd4_sub(uint64_t, uint64_t);

void* fd4_a;
void* fd4_g2 (void** fd4_g2_clos, void** fd4_x1) {
  return (void *)((uint64_t) fd4_x1 + (uint64_t) 1);
}
void* fd4_f;
void* fd4_b;
uint64_t* fd4main() {
  fd4_a = (void *)(({
    fd4_sub( (uint64_t) (uint64_t) (uint64_t) 2 + (uint64_t) 3 + (uint64_t) 4
    , (uint64_t) ({
      void** fd4_x0 = 3;
      (uint64_t) fd4_x0 + (uint64_t) 1;
    }) );
  }));
  fd4_f = (void *)(fd4_mkclosure(fd4_g2, 0));
  fd4_b = (void *)(({
    void** fd4_clos_3 = fd4_f;
    ((void* (*) (void*, void*)) (fd4_clos_3)[0])((void *)fd4_f, (void *)fd4_a);
  }));
  fd4_printn((uint64_t)fd4_b)
  ;
  return 0;
}