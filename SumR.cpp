#include "SumR.h"

void SumR_t::init ( val_t rand_init ) {
  this->__srand(rand_init);
}


int SumR_t::clock ( dat_t<1> reset ) {
  uint32_t min = ((uint32_t)1<<31)-1;
  if (clk_cnt < min) min = clk_cnt;
  clk_cnt-=min;
  if (clk_cnt == 0) clock_hi( reset );
  if (clk_cnt == 0) clock_lo( reset );
  if (clk_cnt == 0) clk_cnt = clk;
  return min;
}


mod_t* SumR_t::clone() {
  mod_t* cloned = new SumR_t(*this);
  return cloned;
}


bool SumR_t::set_circuit_from ( mod_t* src ) {
  SumR_t* mod_typed = dynamic_cast<SumR_t*>(src);
  assert(mod_typed);
  SumR__io_alphai_7 = mod_typed->SumR__io_alphai_7;
  SumR__io_vi_7 = mod_typed->SumR__io_vi_7;
  SumR__io_alphai_6 = mod_typed->SumR__io_alphai_6;
  SumR__io_vi_6 = mod_typed->SumR__io_vi_6;
  SumR__io_alphai_5 = mod_typed->SumR__io_alphai_5;
  SumR__io_vi_5 = mod_typed->SumR__io_vi_5;
  SumR__io_alphai_4 = mod_typed->SumR__io_alphai_4;
  SumR__io_vi_4 = mod_typed->SumR__io_vi_4;
  SumR__io_alphai_3 = mod_typed->SumR__io_alphai_3;
  SumR__io_vi_3 = mod_typed->SumR__io_vi_3;
  SumR__io_alphai_2 = mod_typed->SumR__io_alphai_2;
  SumR__io_vi_2 = mod_typed->SumR__io_vi_2;
  SumR__io_alphai_1 = mod_typed->SumR__io_alphai_1;
  SumR__io_vi_1 = mod_typed->SumR__io_vi_1;
  SumR__io_alphai_0 = mod_typed->SumR__io_alphai_0;
  SumR__io_vi_0 = mod_typed->SumR__io_vi_0;
  SumR__io_sumR = mod_typed->SumR__io_sumR;
  SumR__io_wD = mod_typed->SumR__io_wD;
  SumR__io_wD1 = mod_typed->SumR__io_wD1;
  SumR__io_addToDict = mod_typed->SumR__io_addToDict;
  clk = mod_typed->clk;
  clk_cnt = mod_typed->clk_cnt;
  return true;
}


void SumR_t::print ( FILE* f ) {
}
void SumR_t::print ( std::ostream& s ) {
}


void SumR_t::dump_init ( FILE* f ) {
  fputs("$timescale 1ps $end\n", f);
  fputs("$scope module SumR $end\n", f);
  fputs("$var wire 1 \x21 io_addToDict $end\n", f);
  fputs("$var wire 16 \x22 io_alphai_7 $end\n", f);
  fputs("$var wire 16 \x23 io_vi_7 $end\n", f);
  fputs("$var wire 16 \x24 io_alphai_6 $end\n", f);
  fputs("$var wire 16 \x25 io_vi_6 $end\n", f);
  fputs("$var wire 16 \x26 io_alphai_5 $end\n", f);
  fputs("$var wire 16 \x27 io_vi_5 $end\n", f);
  fputs("$var wire 16 \x28 io_alphai_4 $end\n", f);
  fputs("$var wire 16 \x29 io_vi_4 $end\n", f);
  fputs("$var wire 16 \x2a io_alphai_3 $end\n", f);
  fputs("$var wire 16 \x2b io_vi_3 $end\n", f);
  fputs("$var wire 16 \x2c io_alphai_2 $end\n", f);
  fputs("$var wire 16 \x2d io_vi_2 $end\n", f);
  fputs("$var wire 16 \x2e io_alphai_1 $end\n", f);
  fputs("$var wire 16 \x2f io_vi_1 $end\n", f);
  fputs("$var wire 16 \x30 io_alphai_0 $end\n", f);
  fputs("$var wire 16 \x31 io_vi_0 $end\n", f);
  fputs("$var wire 16 \x32 io_sumR $end\n", f);
  fputs("$var wire 16 \x33 io_wD $end\n", f);
  fputs("$var wire 16 \x34 io_wD1 $end\n", f);
  fputs("$upscope $end\n", f);
  fputs("$enddefinitions $end\n", f);
  fputs("$dumpvars\n", f);
  fputs("$end\n", f);
  fputs("#0\n", f);
  dat_dump<1>(f, SumR__io_addToDict, 0x21);
  SumR__io_addToDict__prev = SumR__io_addToDict;
  dat_dump<1>(f, SumR__io_alphai_7, 0x22);
  SumR__io_alphai_7__prev = SumR__io_alphai_7;
  dat_dump<1>(f, SumR__io_vi_7, 0x23);
  SumR__io_vi_7__prev = SumR__io_vi_7;
  dat_dump<1>(f, SumR__io_alphai_6, 0x24);
  SumR__io_alphai_6__prev = SumR__io_alphai_6;
  dat_dump<1>(f, SumR__io_vi_6, 0x25);
  SumR__io_vi_6__prev = SumR__io_vi_6;
  dat_dump<1>(f, SumR__io_alphai_5, 0x26);
  SumR__io_alphai_5__prev = SumR__io_alphai_5;
  dat_dump<1>(f, SumR__io_vi_5, 0x27);
  SumR__io_vi_5__prev = SumR__io_vi_5;
  dat_dump<1>(f, SumR__io_alphai_4, 0x28);
  SumR__io_alphai_4__prev = SumR__io_alphai_4;
  dat_dump<1>(f, SumR__io_vi_4, 0x29);
  SumR__io_vi_4__prev = SumR__io_vi_4;
  dat_dump<1>(f, SumR__io_alphai_3, 0x2a);
  SumR__io_alphai_3__prev = SumR__io_alphai_3;
  dat_dump<1>(f, SumR__io_vi_3, 0x2b);
  SumR__io_vi_3__prev = SumR__io_vi_3;
  dat_dump<1>(f, SumR__io_alphai_2, 0x2c);
  SumR__io_alphai_2__prev = SumR__io_alphai_2;
  dat_dump<1>(f, SumR__io_vi_2, 0x2d);
  SumR__io_vi_2__prev = SumR__io_vi_2;
  dat_dump<1>(f, SumR__io_alphai_1, 0x2e);
  SumR__io_alphai_1__prev = SumR__io_alphai_1;
  dat_dump<1>(f, SumR__io_vi_1, 0x2f);
  SumR__io_vi_1__prev = SumR__io_vi_1;
  dat_dump<1>(f, SumR__io_alphai_0, 0x30);
  SumR__io_alphai_0__prev = SumR__io_alphai_0;
  dat_dump<1>(f, SumR__io_vi_0, 0x31);
  SumR__io_vi_0__prev = SumR__io_vi_0;
  dat_dump<1>(f, SumR__io_sumR, 0x32);
  SumR__io_sumR__prev = SumR__io_sumR;
  dat_dump<1>(f, SumR__io_wD, 0x33);
  SumR__io_wD__prev = SumR__io_wD;
  dat_dump<1>(f, SumR__io_wD1, 0x34);
  SumR__io_wD1__prev = SumR__io_wD1;
}


void SumR_t::dump ( FILE* f, int t ) {
  if (t == 0) return dump_init(f);
  fprintf(f, "#%d\n", t);
  if (SumR__io_addToDict != SumR__io_addToDict__prev)
    goto L0;
K0:
  if (SumR__io_alphai_7 != SumR__io_alphai_7__prev)
    goto L1;
K1:
  if (SumR__io_vi_7 != SumR__io_vi_7__prev)
    goto L2;
K2:
  if (SumR__io_alphai_6 != SumR__io_alphai_6__prev)
    goto L3;
K3:
  if (SumR__io_vi_6 != SumR__io_vi_6__prev)
    goto L4;
K4:
  if (SumR__io_alphai_5 != SumR__io_alphai_5__prev)
    goto L5;
K5:
  if (SumR__io_vi_5 != SumR__io_vi_5__prev)
    goto L6;
K6:
  if (SumR__io_alphai_4 != SumR__io_alphai_4__prev)
    goto L7;
K7:
  if (SumR__io_vi_4 != SumR__io_vi_4__prev)
    goto L8;
K8:
  if (SumR__io_alphai_3 != SumR__io_alphai_3__prev)
    goto L9;
K9:
  if (SumR__io_vi_3 != SumR__io_vi_3__prev)
    goto L10;
K10:
  if (SumR__io_alphai_2 != SumR__io_alphai_2__prev)
    goto L11;
K11:
  if (SumR__io_vi_2 != SumR__io_vi_2__prev)
    goto L12;
K12:
  if (SumR__io_alphai_1 != SumR__io_alphai_1__prev)
    goto L13;
K13:
  if (SumR__io_vi_1 != SumR__io_vi_1__prev)
    goto L14;
K14:
  if (SumR__io_alphai_0 != SumR__io_alphai_0__prev)
    goto L15;
K15:
  if (SumR__io_vi_0 != SumR__io_vi_0__prev)
    goto L16;
K16:
  if (SumR__io_sumR != SumR__io_sumR__prev)
    goto L17;
K17:
  if (SumR__io_wD != SumR__io_wD__prev)
    goto L18;
K18:
  if (SumR__io_wD1 != SumR__io_wD1__prev)
    goto L19;
K19:
  return;
L0:
  SumR__io_addToDict__prev = SumR__io_addToDict;
  dat_dump<1>(f, SumR__io_addToDict, 0x21);
  goto K0;
L1:
  SumR__io_alphai_7__prev = SumR__io_alphai_7;
  dat_dump<1>(f, SumR__io_alphai_7, 0x22);
  goto K1;
L2:
  SumR__io_vi_7__prev = SumR__io_vi_7;
  dat_dump<1>(f, SumR__io_vi_7, 0x23);
  goto K2;
L3:
  SumR__io_alphai_6__prev = SumR__io_alphai_6;
  dat_dump<1>(f, SumR__io_alphai_6, 0x24);
  goto K3;
L4:
  SumR__io_vi_6__prev = SumR__io_vi_6;
  dat_dump<1>(f, SumR__io_vi_6, 0x25);
  goto K4;
L5:
  SumR__io_alphai_5__prev = SumR__io_alphai_5;
  dat_dump<1>(f, SumR__io_alphai_5, 0x26);
  goto K5;
L6:
  SumR__io_vi_5__prev = SumR__io_vi_5;
  dat_dump<1>(f, SumR__io_vi_5, 0x27);
  goto K6;
L7:
  SumR__io_alphai_4__prev = SumR__io_alphai_4;
  dat_dump<1>(f, SumR__io_alphai_4, 0x28);
  goto K7;
L8:
  SumR__io_vi_4__prev = SumR__io_vi_4;
  dat_dump<1>(f, SumR__io_vi_4, 0x29);
  goto K8;
L9:
  SumR__io_alphai_3__prev = SumR__io_alphai_3;
  dat_dump<1>(f, SumR__io_alphai_3, 0x2a);
  goto K9;
L10:
  SumR__io_vi_3__prev = SumR__io_vi_3;
  dat_dump<1>(f, SumR__io_vi_3, 0x2b);
  goto K10;
L11:
  SumR__io_alphai_2__prev = SumR__io_alphai_2;
  dat_dump<1>(f, SumR__io_alphai_2, 0x2c);
  goto K11;
L12:
  SumR__io_vi_2__prev = SumR__io_vi_2;
  dat_dump<1>(f, SumR__io_vi_2, 0x2d);
  goto K12;
L13:
  SumR__io_alphai_1__prev = SumR__io_alphai_1;
  dat_dump<1>(f, SumR__io_alphai_1, 0x2e);
  goto K13;
L14:
  SumR__io_vi_1__prev = SumR__io_vi_1;
  dat_dump<1>(f, SumR__io_vi_1, 0x2f);
  goto K14;
L15:
  SumR__io_alphai_0__prev = SumR__io_alphai_0;
  dat_dump<1>(f, SumR__io_alphai_0, 0x30);
  goto K15;
L16:
  SumR__io_vi_0__prev = SumR__io_vi_0;
  dat_dump<1>(f, SumR__io_vi_0, 0x31);
  goto K16;
L17:
  SumR__io_sumR__prev = SumR__io_sumR;
  dat_dump<1>(f, SumR__io_sumR, 0x32);
  goto K17;
L18:
  SumR__io_wD__prev = SumR__io_wD;
  dat_dump<1>(f, SumR__io_wD, 0x33);
  goto K18;
L19:
  SumR__io_wD1__prev = SumR__io_wD1;
  dat_dump<1>(f, SumR__io_wD1, 0x34);
  goto K19;
}




void SumR_t::clock_lo ( dat_t<1> reset ) {
  val_t T0;
  { T0 = 0x0L-SumR__io_alphai_7.values[0];}
  T0 = T0 & 0xffffL;
  val_t T1;
  T1 = (SumR__io_alphai_7.values[0] >> 15) & 1;
  val_t T2;
  { T2 = TERNARY_1(T1, T0, SumR__io_alphai_7.values[0]);}
  val_t T3;
  { T3 = 0x0L-SumR__io_vi_7.values[0];}
  T3 = T3 & 0xffffL;
  val_t T4;
  T4 = (SumR__io_vi_7.values[0] >> 15) & 1;
  val_t T5;
  { T5 = TERNARY_1(T4, T3, SumR__io_vi_7.values[0]);}
  val_t T6;
  T6 = T5 * T2;
  val_t T7;
  { T7 = 0x0L-T6;}
  T7 = T7 & 0xffffffffL;
  val_t T8;
  { T8 = T4 ^ T1;}
  val_t T9;
  { T9 = TERNARY_1(T8, T7, T6);}
  val_t T10;
  T10 = sval_t(T9 << 32) >> (32 + 0x8L);
  T10 = T10 & 0xffffffL;
  val_t T11;
  { T11 = 0x0L-SumR__io_alphai_6.values[0];}
  T11 = T11 & 0xffffL;
  val_t T12;
  T12 = (SumR__io_alphai_6.values[0] >> 15) & 1;
  val_t T13;
  { T13 = TERNARY_1(T12, T11, SumR__io_alphai_6.values[0]);}
  val_t T14;
  { T14 = 0x0L-SumR__io_vi_6.values[0];}
  T14 = T14 & 0xffffL;
  val_t T15;
  T15 = (SumR__io_vi_6.values[0] >> 15) & 1;
  val_t T16;
  { T16 = TERNARY_1(T15, T14, SumR__io_vi_6.values[0]);}
  val_t T17;
  T17 = T16 * T13;
  val_t T18;
  { T18 = 0x0L-T17;}
  T18 = T18 & 0xffffffffL;
  val_t T19;
  { T19 = T15 ^ T12;}
  val_t T20;
  { T20 = TERNARY_1(T19, T18, T17);}
  val_t T21;
  T21 = sval_t(T20 << 32) >> (32 + 0x8L);
  T21 = T21 & 0xffffffL;
  val_t T22;
  { T22 = T21+T10;}
  T22 = T22 & 0xffffffL;
  val_t T23;
  { T23 = 0x0L-SumR__io_alphai_5.values[0];}
  T23 = T23 & 0xffffL;
  val_t T24;
  T24 = (SumR__io_alphai_5.values[0] >> 15) & 1;
  val_t T25;
  { T25 = TERNARY_1(T24, T23, SumR__io_alphai_5.values[0]);}
  val_t T26;
  { T26 = 0x0L-SumR__io_vi_5.values[0];}
  T26 = T26 & 0xffffL;
  val_t T27;
  T27 = (SumR__io_vi_5.values[0] >> 15) & 1;
  val_t T28;
  { T28 = TERNARY_1(T27, T26, SumR__io_vi_5.values[0]);}
  val_t T29;
  T29 = T28 * T25;
  val_t T30;
  { T30 = 0x0L-T29;}
  T30 = T30 & 0xffffffffL;
  val_t T31;
  { T31 = T27 ^ T24;}
  val_t T32;
  { T32 = TERNARY_1(T31, T30, T29);}
  val_t T33;
  T33 = sval_t(T32 << 32) >> (32 + 0x8L);
  T33 = T33 & 0xffffffL;
  val_t T34;
  { T34 = 0x0L-SumR__io_alphai_4.values[0];}
  T34 = T34 & 0xffffL;
  val_t T35;
  T35 = (SumR__io_alphai_4.values[0] >> 15) & 1;
  val_t T36;
  { T36 = TERNARY_1(T35, T34, SumR__io_alphai_4.values[0]);}
  val_t T37;
  { T37 = 0x0L-SumR__io_vi_4.values[0];}
  T37 = T37 & 0xffffL;
  val_t T38;
  T38 = (SumR__io_vi_4.values[0] >> 15) & 1;
  val_t T39;
  { T39 = TERNARY_1(T38, T37, SumR__io_vi_4.values[0]);}
  val_t T40;
  T40 = T39 * T36;
  val_t T41;
  { T41 = 0x0L-T40;}
  T41 = T41 & 0xffffffffL;
  val_t T42;
  { T42 = T38 ^ T35;}
  val_t T43;
  { T43 = TERNARY_1(T42, T41, T40);}
  val_t T44;
  T44 = sval_t(T43 << 32) >> (32 + 0x8L);
  T44 = T44 & 0xffffffL;
  val_t T45;
  { T45 = T44+T33;}
  T45 = T45 & 0xffffffL;
  val_t T46;
  { T46 = T45+T22;}
  T46 = T46 & 0xffffffL;
  val_t T47;
  { T47 = 0x0L-SumR__io_alphai_3.values[0];}
  T47 = T47 & 0xffffL;
  val_t T48;
  T48 = (SumR__io_alphai_3.values[0] >> 15) & 1;
  val_t T49;
  { T49 = TERNARY_1(T48, T47, SumR__io_alphai_3.values[0]);}
  val_t T50;
  { T50 = 0x0L-SumR__io_vi_3.values[0];}
  T50 = T50 & 0xffffL;
  val_t T51;
  T51 = (SumR__io_vi_3.values[0] >> 15) & 1;
  val_t T52;
  { T52 = TERNARY_1(T51, T50, SumR__io_vi_3.values[0]);}
  val_t T53;
  T53 = T52 * T49;
  val_t T54;
  { T54 = 0x0L-T53;}
  T54 = T54 & 0xffffffffL;
  val_t T55;
  { T55 = T51 ^ T48;}
  val_t T56;
  { T56 = TERNARY_1(T55, T54, T53);}
  val_t T57;
  T57 = sval_t(T56 << 32) >> (32 + 0x8L);
  T57 = T57 & 0xffffffL;
  val_t T58;
  { T58 = 0x0L-SumR__io_alphai_2.values[0];}
  T58 = T58 & 0xffffL;
  val_t T59;
  T59 = (SumR__io_alphai_2.values[0] >> 15) & 1;
  val_t T60;
  { T60 = TERNARY_1(T59, T58, SumR__io_alphai_2.values[0]);}
  val_t T61;
  { T61 = 0x0L-SumR__io_vi_2.values[0];}
  T61 = T61 & 0xffffL;
  val_t T62;
  T62 = (SumR__io_vi_2.values[0] >> 15) & 1;
  val_t T63;
  { T63 = TERNARY_1(T62, T61, SumR__io_vi_2.values[0]);}
  val_t T64;
  T64 = T63 * T60;
  val_t T65;
  { T65 = 0x0L-T64;}
  T65 = T65 & 0xffffffffL;
  val_t T66;
  { T66 = T62 ^ T59;}
  val_t T67;
  { T67 = TERNARY_1(T66, T65, T64);}
  val_t T68;
  T68 = sval_t(T67 << 32) >> (32 + 0x8L);
  T68 = T68 & 0xffffffL;
  val_t T69;
  { T69 = T68+T57;}
  T69 = T69 & 0xffffffL;
  val_t T70;
  { T70 = 0x0L-SumR__io_alphai_1.values[0];}
  T70 = T70 & 0xffffL;
  val_t T71;
  T71 = (SumR__io_alphai_1.values[0] >> 15) & 1;
  val_t T72;
  { T72 = TERNARY_1(T71, T70, SumR__io_alphai_1.values[0]);}
  val_t T73;
  { T73 = 0x0L-SumR__io_vi_1.values[0];}
  T73 = T73 & 0xffffL;
  val_t T74;
  T74 = (SumR__io_vi_1.values[0] >> 15) & 1;
  val_t T75;
  { T75 = TERNARY_1(T74, T73, SumR__io_vi_1.values[0]);}
  val_t T76;
  T76 = T75 * T72;
  val_t T77;
  { T77 = 0x0L-T76;}
  T77 = T77 & 0xffffffffL;
  val_t T78;
  { T78 = T74 ^ T71;}
  val_t T79;
  { T79 = TERNARY_1(T78, T77, T76);}
  val_t T80;
  T80 = sval_t(T79 << 32) >> (32 + 0x8L);
  T80 = T80 & 0xffffffL;
  val_t T81;
  { T81 = 0x0L-SumR__io_alphai_0.values[0];}
  T81 = T81 & 0xffffL;
  val_t T82;
  T82 = (SumR__io_alphai_0.values[0] >> 15) & 1;
  val_t T83;
  { T83 = TERNARY_1(T82, T81, SumR__io_alphai_0.values[0]);}
  val_t T84;
  { T84 = 0x0L-SumR__io_vi_0.values[0];}
  T84 = T84 & 0xffffL;
  val_t T85;
  T85 = (SumR__io_vi_0.values[0] >> 15) & 1;
  val_t T86;
  { T86 = TERNARY_1(T85, T84, SumR__io_vi_0.values[0]);}
  val_t T87;
  T87 = T86 * T83;
  val_t T88;
  { T88 = 0x0L-T87;}
  T88 = T88 & 0xffffffffL;
  val_t T89;
  { T89 = T85 ^ T82;}
  val_t T90;
  { T90 = TERNARY_1(T89, T88, T87);}
  val_t T91;
  T91 = sval_t(T90 << 32) >> (32 + 0x8L);
  T91 = T91 & 0xffffffL;
  val_t T92;
  { T92 = T91+T80;}
  T92 = T92 & 0xffffffL;
  val_t T93;
  { T93 = T92+T69;}
  T93 = T93 & 0xffffffL;
  val_t T94;
  { T94 = T93+T46;}
  T94 = T94 & 0xffffffL;
  val_t T95;
  { T95 = T94;}
  T95 = T95 & 0xffffL;
  { SumR__io_sumR.values[0] = T95;}
  { val_t __r = this->__rand_val(); SumR__io_wD.values[0] = __r;}
  SumR__io_wD.values[0] = SumR__io_wD.values[0] & 0xffffL;
  { val_t __r = this->__rand_val(); SumR__io_wD1.values[0] = __r;}
  SumR__io_wD1.values[0] = SumR__io_wD1.values[0] & 0xffffL;
}


void SumR_t::clock_hi ( dat_t<1> reset ) {
}


void SumR_api_t::init_mapping_table (  ) {
  dat_table.clear();
  mem_table.clear();
  SumR_t* mod_typed = dynamic_cast<SumR_t*>(module);
  assert(mod_typed);
  dat_table["SumR.io_alphai_7"] = new dat_api<16>(&mod_typed->SumR__io_alphai_7, "SumR.io_alphai_7", "");
  dat_table["SumR.io_vi_7"] = new dat_api<16>(&mod_typed->SumR__io_vi_7, "SumR.io_vi_7", "");
  dat_table["SumR.io_alphai_6"] = new dat_api<16>(&mod_typed->SumR__io_alphai_6, "SumR.io_alphai_6", "");
  dat_table["SumR.io_vi_6"] = new dat_api<16>(&mod_typed->SumR__io_vi_6, "SumR.io_vi_6", "");
  dat_table["SumR.io_alphai_5"] = new dat_api<16>(&mod_typed->SumR__io_alphai_5, "SumR.io_alphai_5", "");
  dat_table["SumR.io_vi_5"] = new dat_api<16>(&mod_typed->SumR__io_vi_5, "SumR.io_vi_5", "");
  dat_table["SumR.io_alphai_4"] = new dat_api<16>(&mod_typed->SumR__io_alphai_4, "SumR.io_alphai_4", "");
  dat_table["SumR.io_vi_4"] = new dat_api<16>(&mod_typed->SumR__io_vi_4, "SumR.io_vi_4", "");
  dat_table["SumR.io_alphai_3"] = new dat_api<16>(&mod_typed->SumR__io_alphai_3, "SumR.io_alphai_3", "");
  dat_table["SumR.io_vi_3"] = new dat_api<16>(&mod_typed->SumR__io_vi_3, "SumR.io_vi_3", "");
  dat_table["SumR.io_alphai_2"] = new dat_api<16>(&mod_typed->SumR__io_alphai_2, "SumR.io_alphai_2", "");
  dat_table["SumR.io_vi_2"] = new dat_api<16>(&mod_typed->SumR__io_vi_2, "SumR.io_vi_2", "");
  dat_table["SumR.io_alphai_1"] = new dat_api<16>(&mod_typed->SumR__io_alphai_1, "SumR.io_alphai_1", "");
  dat_table["SumR.io_vi_1"] = new dat_api<16>(&mod_typed->SumR__io_vi_1, "SumR.io_vi_1", "");
  dat_table["SumR.io_alphai_0"] = new dat_api<16>(&mod_typed->SumR__io_alphai_0, "SumR.io_alphai_0", "");
  dat_table["SumR.io_vi_0"] = new dat_api<16>(&mod_typed->SumR__io_vi_0, "SumR.io_vi_0", "");
  dat_table["SumR.io_sumR"] = new dat_api<16>(&mod_typed->SumR__io_sumR, "SumR.io_sumR", "");
  dat_table["SumR.io_wD"] = new dat_api<16>(&mod_typed->SumR__io_wD, "SumR.io_wD", "");
  dat_table["SumR.io_wD1"] = new dat_api<16>(&mod_typed->SumR__io_wD1, "SumR.io_wD1", "");
  dat_table["SumR.io_addToDict"] = new dat_api<1>(&mod_typed->SumR__io_addToDict, "SumR.io_addToDict", "");
}
