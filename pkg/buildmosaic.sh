#! /bin/bash
mv mosaic*gz Builds
R CMD build mosaic
R CMD check mosaic
sudo R CMD install mosaic
ls -lrthd *.tar.gz
