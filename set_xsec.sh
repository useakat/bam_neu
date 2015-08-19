#!/bin/bash

NCdir=$1
rm -rf xsecNC
#rm -rf xsecCC xsecNC
#cp -rf xsecCC_def xsecCC
cp -rf $NCdir xsecNC
make clean
make