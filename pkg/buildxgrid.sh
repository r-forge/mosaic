#! /bin/bash
mv xgrid*gz Builds
R CMD build xgrid
R CMD check xgrid
sudo R CMD install xgrid
ls -lrthd *.tar.gz
