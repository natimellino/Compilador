#include <inttypes.h>
#include <wchar.h>
extern void *fd4_mkclosure(void*, int, ...);
extern void *fd4_printn(uint64_t);
extern void *fd4_sub(uint64_t, uint64_t);

void* fd4_a;
void* fd4_b;
void* fd4_g5 (void** fd4_g5_clos, void** fd4_n2) {
  return (void *)(({
    void** fd4_m0 = (fd4_g5_clos)[1];
    ({
      void** fd4_rec_mult1_clos = (fd4_g5_clos)[2];
      fd4_n2
      ? (void *)(uint64_t) fd4_m0 + (uint64_t) ({
          void** fd4_clos_4 = ({
            void** fd4_clos_3 = fd4_rec_mult1_clos;
            ((void* (*) (void*, void*)) (fd4_clos_3)[0])( (void *)fd4_rec_mult1_clos
            , (void *)fd4_m0 );
          });
          ((void* (*) (void*, void*)) (fd4_clos_4)[0])( (void *)({
            void** fd4_clos_3 = fd4_rec_mult1_clos;
            ((void* (*) (void*, void*)) (fd4_clos_3)[0])( (void *)fd4_rec_mult1_clos
            , (void *)fd4_m0 );
          })
          , (void *)({
            fd4_sub((uint64_t) fd4_n2, (uint64_t) 1);
          }) );
        })
      : (void *)0;
    });
  }));
}
void* fd4_rec_mult1 (void** fd4_rec_mult1_clos, void** fd4_m0) {
  return (void *)(fd4_mkclosure(fd4_g5, 2, fd4_m0, fd4_rec_mult1_clos));
}
void* fd4_mult;
void* fd4_rec_fact7 (void** fd4_rec_fact7_clos, void** fd4_n6) {
  return (void *)(({
    void** fd4_mult = (fd4_rec_fact7_clos)[1];
    fd4_n6
    ? (void *)({
        void** fd4_clos_10 = ({
          void** fd4_clos_8 = fd4_mult;
          ((void* (*) (void*, void*)) (fd4_clos_8)[0])( (void *)fd4_mult
          , (void *)fd4_n6 );
        });
        ((void* (*) (void*, void*)) (fd4_clos_10)[0])( (void *)({
          void** fd4_clos_8 = fd4_mult;
          ((void* (*) (void*, void*)) (fd4_clos_8)[0])( (void *)fd4_mult
          , (void *)fd4_n6 );
        })
        , (void *)({
          void** fd4_clos_9 = fd4_rec_fact7_clos;
          ((void* (*) (void*, void*)) (fd4_clos_9)[0])( (void *)fd4_rec_fact7_clos
          , (void *)({
            fd4_sub((uint64_t) fd4_n6, (uint64_t) 1);
          }) );
        }) );
      })
    : (void *)1;
  }));
}
void* fd4_fact;
void* fd4_c;
uint64_t* fd4main() {
  fd4_a = (void *)(3);
  fd4_b = (void *)(20);
  fd4_mult = (void *)(fd4_mkclosure(fd4_rec_mult1, 0));
  fd4_fact = (void *)(fd4_mkclosure(fd4_rec_fact7, 1, fd4_mult));
  fd4_c = (void *)((uint64_t) ({
    void** fd4_clos_12 = fd4_fact;
    ((void* (*) (void*, void*)) (fd4_clos_12)[0])( (void *)fd4_fact
    , (void *)({
      void** fd4_clos_11 = fd4_fact;
      ((void* (*) (void*, void*)) (fd4_clos_11)[0])( (void *)fd4_fact
      , (void *)fd4_a );
    }) );
  }) + (uint64_t) fd4_b);
  fd4_printn((uint64_t)fd4_c)
  ;
  return 0;
}