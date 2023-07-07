#!/bin/bash

VER=$(grep Version DESCRIPTION)
VER=${VER//*Version: /}
echo $VER

cd ..
rm PolicySimulator/inst/app/*.tar.gz
R CMD build PolicySimulator
mv PolicySimulator_$VER.tar.gz PolicySimulator/inst/app
