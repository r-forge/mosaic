#! /bin/bash
export _R_CHECK_FORCE_SUGGESTS_=false
mv mosaic*gz Builds
R CMD build --resave-data mosaic
R CMD check mosaic
sudo R CMD install mosaic
ls -lrthd *.tar.gz
