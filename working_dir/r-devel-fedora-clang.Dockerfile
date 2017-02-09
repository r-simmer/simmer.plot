## Build: docker build -t r-devel-fedora-clang -f simmer.plot/working_dir/r-devel-fedora-clang.Dockerfile .
## Usage: docker run --rm -ti -v $(pwd):/mnt r-devel-fedora-clang R CMD check --as-cran /mnt/simmer_x.x.x.tar.gz

## Start with the Docker fedora base image
FROM fedora:latest
MAINTAINER Iñaki Úcar <i.ucar86@gmail.com>

## Set a useful default locale
ENV LANG=en_US.utf-8

## Install & Update packages
RUN dnf update -y && dnf install -y \
  automake \
  binutils \
  bison \
  blas-devel \
  bzip2-devel \
  cairo-devel \
  clang \
  findutils \
  gcc-gfortran \
  git \
  java \
  lapack-devel \
  libcurl-devel \
  libjpeg-turbo-devel \
  libssh2-devel \
  libstdc++-static \
  libtiff-devel \
  libxml2-devel \
  libX11-devel \
  libXt-devel \
  make \
  pango-devel \
  pcre-devel \
  pkgconfig \
  qpdf \
  readline-devel \
  svn \
  tar \
  tcl-devel \
  tex \
  texinfo \
  texlive \
  texlive-inconsolata \
  texlive-latex-bin-bin \
  tk-devel \
  unzip \
  which \
  xdg-utils \
  xz-devel \
  zip \
  zlib-devel \
  && dnf clean all

## Check out R-devel, build, install, clean
RUN cd /tmp \
  && svn co https://svn.r-project.org/R/trunk R-devel
RUN cd /tmp/R-devel \
  && CC="clang" \
     CXX="clang++" \
     CXX1X="clang++" \
     FC="gfortran" \
     F77="gfortran" \
     ./configure --enable-R-shlib \
       --without-blas \
       --without-lapack \
       --with-readline \
       --without-recommended-packages \
       --disable-openmp \
  && make \
  && make install \
  && cd /tmp \
  && rm -rf R-devel

## Set default CRAN repo, Makevars
RUN echo 'options("repos"="http://cran.rstudio.com")' >> /usr/local/lib64/R/etc/Rprofile.site

## Install dependencies
RUN Rscript -e 'install.packages(c("devtools", "simmer.plot"))'
RUN Rscript -e 'install.packages(c("testthat", "knitr", "rmarkdown", "covr"))'
