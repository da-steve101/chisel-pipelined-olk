#include "SumR.h"

int main (int argc, char* argv[]) {
  SumR_t* module = new SumR_t();
  module->init();
  SumR_api_t* api = new SumR_api_t();
  api->init(module);
  FILE *f = fopen("./SumR.vcd", "w");
  FILE *tee = NULL;  module->set_dumpfile(f);
  api->set_teefile(tee);
  api->read_eval_print_loop();
  if (f) fclose(f);
  if (tee) fclose(tee);
}
