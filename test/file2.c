#include <inttypes.h>
#include <wchar.h>
extern void *fd4_mkclosure(void*, int, ...);
extern void *fd4_printn(uint64_t);
extern void *fd4_sub(uint64_t, uint64_t);

void* fd4_g5 (void** fd4_g5_clos, void** fd4_n2) {
  return (void *)(({
    void** fd4_m0 = (fd4_g5_clos)[1];
    ({
      void** fd4_rec_suma1_clos = (fd4_g5_clos)[2];
      fd4_n2
      ? (void *)({
          void** fd4_clos_4 = ({
            void** fd4_clos_3 = fd4_rec_suma1_clos;
            ((void* (*) (void*, void*)) (fd4_clos_3)[0])( (void *)fd4_rec_suma1_clos
            , (void *)(uint64_t) 1 + (uint64_t) fd4_m0 );
          });
          ((void* (*) (void*, void*)) (fd4_clos_4)[0])( (void *)({
            void** fd4_clos_3 = fd4_rec_suma1_clos;
            ((void* (*) (void*, void*)) (fd4_clos_3)[0])( (void *)fd4_rec_suma1_clos
            , (void *)(uint64_t) 1 + (uint64_t) fd4_m0 );
          })
          , (void *)({
            fd4_sub((uint64_t) fd4_n2, (uint64_t) 1);
          }) );
        })
      : (void *)fd4_m0;
    });
  }));
}
void* fd4_rec_suma1 (void** fd4_rec_suma1_clos, void** fd4_m0) {
  return (void *)(fd4_mkclosure(fd4_g5, 2, fd4_m0, fd4_rec_suma1_clos));
}
void* fd4_suma;
void* fd4_a;
uint64_t* fd4main() {
  fd4_suma = (void *)(fd4_mkclosure(fd4_rec_suma1, 0));
  fd4_a = (void *)(({
    void** fd4_clos_7 = ({
      void** fd4_clos_6 = fd4_suma;
      ((void* (*) (void*, void*)) (fd4_clos_6)[0])((void *)fd4_suma, (void *)2);
    });
    ((void* (*) (void*, void*)) (fd4_clos_7)[0])( (void *)({
      void** fd4_clos_6 = fd4_suma;
      ((void* (*) (void*, void*)) (fd4_clos_6)[0])((void *)fd4_suma, (void *)2);
    })
    , (void *)3 );
  }));
  fd4_printn((uint64_t)fd4_a)
  ;
  return 0;
}