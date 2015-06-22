#ifndef __SumR__
#define __SumR__

#include "emulator.h"

class SumR_t : public mod_t {
 private:
  val_t __rand_seed;
  void __srand(val_t seed) { __rand_seed = seed; }
  val_t __rand_val() { return ::__rand_val(&__rand_seed); }
 public:
  dat_t<1> SumR__io_addToDict;
  dat_t<16> SumR__io_alphai_7;
  dat_t<16> SumR__io_vi_7;
  dat_t<16> SumR__io_alphai_6;
  dat_t<16> SumR__io_vi_6;
  dat_t<16> SumR__io_alphai_5;
  dat_t<16> SumR__io_vi_5;
  dat_t<16> SumR__io_alphai_4;
  dat_t<16> SumR__io_vi_4;
  dat_t<16> SumR__io_alphai_3;
  dat_t<16> SumR__io_vi_3;
  dat_t<16> SumR__io_alphai_2;
  dat_t<16> SumR__io_vi_2;
  dat_t<16> SumR__io_alphai_1;
  dat_t<16> SumR__io_vi_1;
  dat_t<16> SumR__io_alphai_0;
  dat_t<16> SumR__io_vi_0;
  dat_t<16> SumR__io_sumR;
  dat_t<16> SumR__io_wD;
  dat_t<16> SumR__io_wD1;
  dat_t<1> SumR__io_addToDict__prev;
  dat_t<16> SumR__io_alphai_7__prev;
  dat_t<16> SumR__io_vi_7__prev;
  dat_t<16> SumR__io_alphai_6__prev;
  dat_t<16> SumR__io_vi_6__prev;
  dat_t<16> SumR__io_alphai_5__prev;
  dat_t<16> SumR__io_vi_5__prev;
  dat_t<16> SumR__io_alphai_4__prev;
  dat_t<16> SumR__io_vi_4__prev;
  dat_t<16> SumR__io_alphai_3__prev;
  dat_t<16> SumR__io_vi_3__prev;
  dat_t<16> SumR__io_alphai_2__prev;
  dat_t<16> SumR__io_vi_2__prev;
  dat_t<16> SumR__io_alphai_1__prev;
  dat_t<16> SumR__io_vi_1__prev;
  dat_t<16> SumR__io_alphai_0__prev;
  dat_t<16> SumR__io_vi_0__prev;
  dat_t<16> SumR__io_sumR__prev;
  dat_t<16> SumR__io_wD__prev;
  dat_t<16> SumR__io_wD1__prev;
  int clk;
  int clk_cnt;

  void init ( val_t rand_init = 0 );
  void clock_lo ( dat_t<1> reset );
  void clock_hi ( dat_t<1> reset );
  int clock ( dat_t<1> reset );
  mod_t* clone();
  bool set_circuit_from(mod_t* src);
  void print ( FILE* f );
  void print ( std::ostream& s );
  void dump ( FILE* f, int t );
  void dump_init ( FILE* f );

};

class SumR_api_t : public mod_api_t {
  void init_mapping_table();
};



#endif
