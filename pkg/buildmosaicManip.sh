#! /bin/bash
export _R_CHECK_FORCE_SUGGESTS_=false
mv mosaicManip*gz Builds
R CMD build --resave-data mosaicManip
R CMD check mosaicManip
sudo R CMD install mosaicManip
ls -lrthd *.tar.gz
