#!/usr/bin/env bash

R CMD build psychophysics/
R CMD INSTALL -l ~/.R_library psychophysics_1.0.4.tar.gz
rm psychophysics.pdf
R CMD Rd2pdf psychophysics/


