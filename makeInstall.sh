#!/usr/bin/env bash

R CMD build AudioUtils/
R CMD INSTALL -l ~/.R_library AudioUtils_1.0.tar.gz
rm AudioUtils.pdf
R CMD Rd2pdf AudioUtils/


