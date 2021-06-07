#include <stdio.h>
#include <stdlib.h>

#define YCF_YIELD_NO_REDS()

static int f_2(char* name, int n) {
  for(int y = 0; y < 2; y++) {
    printf("%s f_2: y=%d\n", name, y);
  }
  return n*2;
}

static void f_1(char* name, int x) {
  YCF_YIELD_NO_REDS();
  while (x > 0) {
    int f_2_ret = f_2(name, x);
    printf("%s f_1: x=%d f_2_ret=%d\n", name, x, f_2_ret);
    x--;
  }
  printf("%s f_1: DONE\n", name);
}

static void* ycf_alloc(size_t size, void* context){
  (void)context;
  return malloc(size);
}

static void ycf_free(void* data, void* context){
  (void)context;
  free(data);
}

int main( int argc, const char* argv[] ) {
#ifdef YCF_YIELD_CODE_GENERATED
  long t1_nr_of_reds = 1;
  void* t1_state = NULL;
  long t2_nr_of_reds = 2;
  void* t2_state = NULL;
  long t3_nr_of_reds = 1000;
  void* t3_state = NULL;
  /* Start t1, t2  and t3*/
  f_1_ycf_gen_yielding(&t1_nr_of_reds, &t1_state, NULL,
                       ycf_alloc, ycf_free, NULL, 0, NULL,
                       "t1", 2);
  f_1_ycf_gen_yielding(&t2_nr_of_reds, &t2_state, NULL,
                       ycf_alloc, ycf_free, NULL, 0, NULL,
                       "t2", 4);
  f_1_ycf_gen_yielding(&t3_nr_of_reds, &t3_state, NULL,
                       ycf_alloc, ycf_free, NULL, 0, NULL,
                       "t3", 2);
  printf("THREADS STARTED\n");
  /* Execute t1, t2 and t3*/
  while (t1_state != NULL ||
         t2_state != NULL ||
         t3_state != NULL) {
    t1_nr_of_reds = 1;
    t2_nr_of_reds = 2;
    if (t1_state != NULL) {
      printf("SCHEDULING THREAD: t1\n");
      f_1_ycf_gen_continue(&t1_nr_of_reds, &t1_state, NULL);
      if (t1_state == NULL) {
        printf("THREAD t1 DONE (number of reductions left = %ld)\n",
               t1_nr_of_reds);
      }
    }
    if (t2_state != NULL) {
      printf("SCHEDULING THREAD: t2\n");
      f_1_ycf_gen_continue(&t2_nr_of_reds, &t2_state, NULL);
      if (t2_state == NULL) {
        printf("THREAD t2 DONE (number of reductions left = %ld)\n",
               t2_nr_of_reds);
      }
    }
    if (t3_state != NULL) {
      printf("SCHEDULING THREAD: t3\n");
      f_1_ycf_gen_continue(&t3_nr_of_reds, &t3_state, NULL);
      if (t3_state == NULL) {
        printf("THREAD t3 DONE (number of reductions left = %ld)\n",
               t3_nr_of_reds);
      }
    }
  }
#endif
  (void)f_1;
  printf("DONE\n");
  return 0;
}

