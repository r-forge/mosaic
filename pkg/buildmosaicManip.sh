#! /bin/bash
mv mosaicManip*gz Builds
R CMD build mosaicManip
R CMD check mosaicManip
sudo R CMD install mosaicManip
ls -lrthd *.tar.gz
