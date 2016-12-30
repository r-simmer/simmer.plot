## Patch release

Sorry for sending another patch, stupid error by my side... This patch DEFINITELY solves the ERROR in some builds. In all my tested platforms, null pointers were printed as "0", but I didn't realise that this is implementation-dependent, and it could be "(nil)", it could be other things.

## Test environments

* Fedora 25 + GCC + clang (local), R 3.3.2
* Ubuntu 12.04 + GCC (on travis-ci), R 3.2.5, 3.3.1, devel
* win-builder, R devel

## R CMD check results

There were no ERRORs or WARNINGs.

There were 1 NOTEs:

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Iñaki Ucar <i.ucar86@gmail.com>’

License components with restrictions and base license permitting such:
  MIT + file LICENSE
File 'LICENSE':
  YEAR: 2016
  COPYRIGHT HOLDER: Bart Smeets, Iñaki Ucar

## Downstream dependencies

There are currently no downstream dependencies for this package.
