#! /bin/bash
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
sudo R CMD install mosaic*gz
mv mosaic*gz Builds
ls -lrth Builds
