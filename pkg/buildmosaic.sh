#! /bin/bash
R CMD build mosaicData
R CMD build mosaicCalc
R CMD build mosaicStat
R CMD build mosaic
R CMD check mosaicData
R CMD check mosaicCalc
R CMD check mosaicStat
R CMD check mosaic
sudo R CMD install mosaicData*gz
sudo R CMD install mosaicCalc*gz
sudo R CMD install mosaicStat*gz
sudo R CMD install mosaic*gz
mv mosaic*gz Builds
ls -lrth Builds
