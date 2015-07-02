#! /bin/bash

if [ ! -d "caret" ]; then
  git clone https://github.com/da-steve101/caret.git
  #remove caret doc for faster build
  rm -f caret/pkg/caret/vignettes/caret.Rnw
fi
if [ ! -d "kernlab" ]; then
  git clone https://github.com/da-steve101/kernlab.git
  #remove kernlab doc for faster build
  rm -f kernlab/vignettes/kernlab.Rnw
fi

mkdir -p caretInst
mkdir -p kernlabInst

# generate models for caret
cd caret/models && Rscript parseModels.R
cd $OLDPWD

# build + install kernlab and caret
R CMD build caret/pkg/caret
R CMD INSTALL -l caretInst/ caret_*.tar.gz
R CMD build kernlab/
R CMD INSTALL -l kernlabInst/ kernlab_*.tar.gz
