#! /bin/bash
mv mosaic*gz Builds
R CMD build mosaicCore
R CMD build mosaicData
R CMD build mosaicCalc
R CMD build mosaicStat
R CMD build mosaic
R CMD check mosaicCore
R CMD check mosaicData
R CMD check mosaicCalc
R CMD check mosaicStat
R CMD check mosaic
sudo R CMD install mosaicCore
sudo R CMD install mosaicData
sudo R CMD install mosaicCalc
sudo R CMD install mosaicStat
sudo R CMD install mosaic
ls -lrthd *.tar.gz
