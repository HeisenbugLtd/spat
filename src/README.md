# Source file structure

I decided to restructure the source files a bit, with the number of files
steadily rising, the flat approach became unusable.

The root package (`spat.ads`, `spat.adb`) remains at the top level. The
remaining files are split into the following subdirectories:

* `app/`
  Files relatively specific to the `run_spat` application (i.e. command line
  parsing and the specific output subroutines).

* `util/`
  These are sources that are not directly involved with what spat does, but do
  provide needed supporting functionality (collecting files, a stop watch
  implementation for performance output, logging, and low-level data
  structures).
  
* `core/`
  These are sources that are the core of parsing, analysing and reporting data.
