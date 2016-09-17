# Open source of NEAR Orbit viewer

Near-Earth Asteroid Rendezvous (NEAR) spacecraft sequence planning tool

## Setup (in src/):

* Pre-requisites:  libX11 libXt libXm libXmu

* Activate one of the Makefile.\* files to HOWTOFORT
  * e.g. echo HOWTOFORT=gfortran_linux > Makefile.howtofort
    * N.B. this is the default for a freshly cloned git repository

* Point the naif symlink to someplace where the SPICE FORTRAN toolkit resides
  * e.g. rm -f naif ; ln -s ~/toolkit naif
  * The point is that NEARTOP/naif/lib/spicelib.a should be the SPICE FORTRAN toolkit library


## Build SPUD2STL (in src/):

* Do Setup (above)
* make spud2stl


Brian T. Carcich

BrianTCarcich@gmail.com

2016-09-17
