![](https://raw.githubusercontent.com/HeisenbugLtd/heisenbugltd.github.io/master/assets/img/spat/cover.png)

# SPARK Proof Analysis Tool

## Introduction

The SPARK tools (i.e. GNATprove) leave behind a trove of information after a
proof run. `spat` is intended to take these information and extract some useful
information from it (like for example, where the provers spent their time,
which provers solved the problem first etc. pp.).
In a way, this tool serves a similar purpose as the
[gnatprove2xls](https://github.com/damaki/gnatprove2xls)
tool, but it parses the "raw" data instead of post-processed output, hence it
has more information available, and it's written in Ada. (I considered using
Python, but rejected it, because that would have been too easy).

## Motivation

The idea is that making use of that information will help identify and fix
bottlenecks during proof. As the format of these files is virtually
undocumented, a little bit of reverse engineering may be required, but on the
other hand, maybe the result is actual
[documentation](https://github.com/HeisenbugLtd/spat/blob/master/doc/spark_file_format.md).

## Compiling the tool

### Requirements

You need a recent version of GNAT with the GNATCOLL library. GNAT CE 2019 or
GNAT CE 2020 should suffice.

You also need the [si_units](https://github.com/HeisenbugLtd/si_units) library
to be installed.

### Step by step instructions

Note that the instructions are currently for Linux only, but installing it on
Windows should be similarly straightforward. I also assume that SPARK users are
familiar with compiling Ada code, so installing it on Windows shouldn't be an
issue.

#### Compile and install SI_Units

* Clone the SI_Units repository: `git clone https://github.com/HeisenbugLtd/si_units`
* Compile the SI_Units library: `gprbuild -p -P si_units/si_units_lib.gpr`
* Install the SI Units library: `gprinstall -f -p -P si_units/si_units_lib.gpr`

Depending on how your GNAT installation is set up, the latter command may
require elevated privileges to write into the installation directory, so if
needed, prepend `sudo </path/to/gnat/installation/>/bin/` to the `gprinstall`
instruction above.

#### Compile and install SPAT

* Clone the SPAT repository: `git clone https://github.com/HeisenbugLtd/spat`
* Compile it: `gprbuild -p -P spat/spat.gpr`
* Install it: `gprinstall -f -p -P spat/spat.gpr`

Depending on how your GNAT installation is set up, the latter command may
require elevated privileges to write into the installation directory, so if
needed, prepend `sudo </path/to/gnat/installation/>/bin/` to the `gprinstall`
instruction above.

After that, the **`run_spat`** executable should be installed in your GNAT
installation and is ready to use.

## Invoking the tool

Like many other GNAT related tools, `spat` is designed to run against a GNAT project file (`. gpr`) to collect the information about the source files in the project. (An earlier version used a directory only search, but that seemed inconvenient and error-prone).

### Command line

Quick help:

`run_spat -h`

will give you a quick overview over the available command line options:

```
usage: run_spat [--help|-h] [--list|-l] [--summary|-s] [--failed-only|-f] 
               [--unproved|-u] [--details|-d] [--sort-by|-c SORT-BY] 
               [--verbose|-v] [--project|-P PROJECT] 

Parses .spark files and outputs information about them.

positional arguments:
   
optional arguments:
   --help, -h            Show this help message
   --list, -l            List entities
   --summary, -s         List summary (per file)
   --failed-only, -f     Show failed attempts only
   --unproved, -u        Show unproved attempts only
   --details, -d         Show details for entities
   --sort-by, -c         Sort output (SORT-BY: a = alphabetical, t = by time)
   --verbose, -v         Verbose (tracing) output
   --project, -P         GNAT project file (.gpr) (mandatory!)
```

The `--project` argument is the only argument that is not optional, but without
a `--list`, or `--summary` argument, `run_spat` will not output anything
useful.  It will still try to parse the files it finds, though.

### The `--summary` option

This option is intended to show a quick summary of the files analyzed. A
typical output would look like this:

```
test_phelix_api.spark                    => (Flow  => 15.9 ms,
                                             Proof => 12.3 s)
test_phelix.spark                        => (Flow  => 741.0 µs,
                                             Proof => 3.0 µs)
saatana-crypto.spark                     => (Flow  => 7.4 ms,
                                             Proof => 4.7 s)
saatana-crypto-stream_tools.spark        => (Flow  => 55.0 µs,
                                             Proof => 2.0 µs)
saatana-crypto-lemmas.spark              => (Flow  => 1.6 ms,
                                             Proof => 2.8 s)
saatana-crypto-phelix.spark              => (Flow  => 209.6 ms,
                                             Proof => 344.3 s)
saatana-crypto-phelix-test_vectors.spark => (Flow  => 29.0 µs,
                                             Proof => 3.0 µs)
saatana.spark                            => (Flow  => 343.0 µs,
                                             Proof => 4.0 µs)
```

You can use the `--sort-by` option with `--summary`, either for an alphabetical
list or a list sorted by time (descending order, so files with the most time
needed by the provers come first). By default, no particular order is imposed
on the output.

*Note* that currently the `--details`, `--failed`, and `--unproved` options
have no effect on the output here, these options are designed to work with the
`--list` option only.

### The `--list` option

This is the main mode the tool is designed to be run in. It outputs the list of
entities (i.e. Ada language identifiers) it finds in the `.spark` files. By
default, the output has no particular order, but as mentioned in the previous
chapter, with the `--sort-by` option you can force one.

Run the command:

```sh
run_spat -ct -l -P saatana.gpr
```

Typical output looks like this:

```
Saatana.Crypto.Phelix.H                    => 303.8 s/304.3 s
Saatana.Crypto.Phelix.Setup_Key            => 130.4 s/131.2 s
Saatana.Crypto.Phelix.Encrypt_Bytes        => 96.2 s/96.9 s
Saatana.Crypto.Phelix.Finalize             => 67.7 s/67.8 s
Saatana.Crypto.Phelix.Setup_Nonce          => 390.0 ms/2.6 s
Saatana.Crypto.Phelix.Decrypt_Bytes        => 1.6 s/2.2 s
Saatana.Crypto.Phelix.Process_AAD          => 220.0 ms/790.0 ms
Saatana.Crypto.Lemmas                      => 590.0 ms/590.0 ms
Test_Phelix_API                            => 150.0 ms/340.0 ms
Saatana.Crypto.Phelix.Exclusive_Or         => 190.0 ms/190.0 ms
Saatana.Crypto.Phelix.Decrypt_Packet       => 60.0 ms/180.0 ms
...
```

The first value is the longest proof time, the second value is the total sum of
all proof times for this entity.

#### The `--failed-only` option

When invoked together with the `--list` option, it will only show proof
attempts where at least one prover failed to prove the verification condition.
This does not necessarily mean that the proof itself failed. Especially, if a
different prover could prove the condition later, this is a good indicator to
look if the call order of the provers should be changed.

Example:

```sh
run_spat -ct -l -f -P saatana.gpr
```

Typical output:

```
Saatana.Crypto.Phelix.H                    => 303.8 s/304.3 s
Saatana.Crypto.Phelix.Setup_Key            => 130.4 s/131.2 s
```

Here, we can see that there are two entities where a prover failed to prove the
verification condition.

#### The `--unproved` option

When invoked together with the `--list` option, it will only show proof
attempts where all provers failed to prove the verification condition.

Example:

```sh
run_spat -ct -l -u -P sparknacl.gpr
```

Typical output:

```
SPARKNaCl.Sign.Sign                                       => 134.7 s/293.7 s
SPARKNaCl.ASR_8                                           => 18.1 s/18.1 s
SPARKNaCl.ASR_16                                          => 14.5 s/14.5 s
SPARKNaCl.ASR_4                                           => 6.2 s/6.2 s
SPARKNaCl.Car.Nearlynormal_To_Normal                      => 1.3 s/3.0 s
```

Here, we can see that there are five entities with unproven verification
conditions.

#### The `--details` option

When invoked together with the `--list` option, it will show all the individual
proof attempts/paths for an entity.

Example (with `--failed-only`):

```sh
run_spat -ct -l -d -f -P saatana.gpr
```

Output:

```
Saatana.Crypto.Phelix.H                    => 303.8 s/304.3 s
`-VC_POSTCONDITION saatana-crypto-phelix.adb:49:17 => 303.8 s/304.3 s
 `-Z3: 303.8 s (Unknown (unknown))
  -CVC4: 480.0 ms (Valid)
Saatana.Crypto.Phelix.Setup_Key            => 130.4 s/131.2 s
`-VC_RANGE_CHECK saatana-crypto-phelix.adb:461:44 => 130.4 s/131.2 s
 `-Z3: 130.4 s (Unknown (unknown))
  -CVC4: 780.0 ms (Valid)
```

As above, but here you can see the individual proof results. It seems that Z3
is not well suited to prove these particular verification conditions, but CVC4
can prove them quite fast. This is a good indicator that in that particular
case, CVC4 should be called first to optimize proof times.

Without the `--failed-only` option, all proof attempts will be shown in a
similar manner.

Please keep in mind that a single proof may have multiple paths leading to it,
resulting in more than just one proof attempt for a single verification
condition.

Another example (with `--unproved`):

```sh
run_spat -ct -l -d -u -P sparknacl.gpr
```

Typical output:

```
SPARKNaCl.Sign.Sign                                       => 31.3 s/71.8 s
`-VC_OVERFLOW_CHECK sparknacl-sign.adb:890:36 => 31.3 s/71.8 s
 `-CVC4: 31.3 s (Unknown (unknown))
  -Z3: 5.3 s (Unknown (unknown))
 `-CVC4: 30.7 s (Unknown (unknown))
  -Z3: 4.5 s (Unknown (unknown))
SPARKNaCl.ASR_16                                          => 5.7 s/5.7 s
`-VC_POSTCONDITION sparknacl.ads:355:35 => 5.7 s/5.7 s
 `-Z3: 5.7 s (Unknown (unknown))
  -CVC4: 0.0 s (Unknown (unknown))
Justified with: "From definition of arithmetic shift right".
SPARKNaCl.ASR_8                                           => 3.5 s/3.5 s
`-VC_POSTCONDITION sparknacl.ads:367:35 => 3.5 s/3.5 s
 `-Z3: 3.5 s (Unknown (unknown))
  -CVC4: 0.0 s (Unknown (unknown))
Justified with: "From definition of arithmetic shift right".
SPARKNaCl.Car.Nearlynormal_To_Normal                      => 1.3 s/3.1 s
`-VC_LOOP_INVARIANT_PRESERV sparknacl-car.adb:324:13 => 1.3 s/1.9 s
 `-CVC4: 1.3 s (Unknown (unknown))
  -Z3: 600.0 ms (Unknown (unknown))
`-VC_ASSERT sparknacl-car.adb:343:31 => 740.0 ms/1.2 s
 `-Z3: 740.0 ms (Unknown (unknown))
  -CVC4: 410.0 ms (Unknown (unknown))
SPARKNaCl.ASR_4                                           => 1.3 s/1.3 s
`-VC_POSTCONDITION sparknacl.ads:379:35 => 1.3 s/1.3 s
 `-Z3: 1.3 s (Unknown (unknown))
  -CVC4: 0.0 s (Unknown (unknown))
Justified with: "From definition of arithmetic shift right".
```

As above, but here you can see the individual proof results including any
justification messages (if present).

#### The `--verbose` option

This option is mainly used for debugging, it enables extra output about what
`run_spat` is doing (i.e. files found in the given project file, parse results
and timings).
