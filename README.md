![](https://raw.githubusercontent.com/HeisenbugLtd/heisenbugltd.github.io/master/assets/img/spat/cover.png)

# SPARK Proof Analysis Tool

[![Build Linux](https://github.com/HeisenbugLtd/spat/workflows/Build%20Linux/badge.svg)](https://github.com/HeisenbugLtd/spat/actions?query=workflow%3A%22Build+Linux%22)

## Content
* ### 1. [Introduction](README.md#introduction)
* ### 2. [Motivation](README.md#motivation)
* ### 3. [Compiling the Tool](README.md#compiling-the-tool)
  * #### 3.1 [Requirements](README.md#requirements)
  * #### 3.2 [Step by Step Instructions](README.md#step-by-step-instructions)
* ### 4. [Invoking the Tool](README.md#invoking-the-tool)
  * #### 4.1 [Command Line](README.md#command-line)
  * #### 4.2 [The `--summary` option](README.md#the---summary-option)
  * #### 4.3 [The `--report-mode` option](README.md#the---report-mode-option)
  * #### 4.4 [The `--details` option](README.md#the---details-option)
  * #### 4.5 [The `--cut-off` option](README.md#the---cut-off-option)
  * #### 4.6 [The `--verbose` option](README.md#the---verbose-option)
  * #### 4.7 [The `--version` option](README.md#the---version-option)
* ### 5. [Tool Limitations](README.md#tool-limitations)

## Introduction

The SPARK tools (i.e. GNATprove) leave behind a trove of information after a
proof run. `spat` is intended to take these information and extract some useful
information from it (like for example, where the provers spent their time,
which provers solved the problem first etc. pp.).  In a way, this tool serves a
similar purpose as the [gnatprove2xls](https://github.com/damaki/gnatprove2xls)
tool, but it parses the "raw" data instead of post-processed output, hence it
has more information available, and it's written in Ada. (I considered using
Python, but rejected it, because that would have been too easy).

## Motivation

The idea is that making use of that information will help identify and fix
bottlenecks during proof.  As the format of these files is virtually
undocumented, a little bit of reverse engineering may be required, but on the
other hand, maybe the result is actual
[documentation](https://github.com/HeisenbugLtd/spat/blob/master/doc/spark_file_format.md).

## Compiling the tool

### Requirements

You need a recent version of GNAT with the GNATCOLL library.  GNAT CE 2019 or
GNAT CE 2020 should suffice.

You also need the [si_units](https://github.com/HeisenbugLtd/si_units) library
to be installed.

***Please note*** Currently the SI_Units library does not seem to compile with
some GNAT FSF versions.  The compiler gets stuck and never finishes.  Hence,
the GNAT Community Release is recommended, but you may try if it works for you.

### Step by step instructions

Note that the instructions are currently for Linux only, but installing it on
Windows should be similarly straightforward.  I also assume that SPARK users
are familiar with compiling Ada code, so installing it on Windows shouldn't be
an issue.

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

Like many other GNAT related tools, `spat` is designed to run against a GNAT
project file (`. gpr`) to collect the information about the source files in the
project.

### Command line

Quick help:

`run_spat -h`

will give you a quick overview over the available command line options:

```
usage: run_spat [--help|-h] [--project|-P PROJECT] [--summary|-s]
               [--report-mode|-r REPORT-MODE] [--sort-by|-c SORT-BY]
               [--cut-off|-p CUT-OFF] [--details|-d] [--version|-V]
               [--verbose|-v]

Parses .spark files and outputs information about them.

positional arguments:

optional arguments:
   --help, -h            Show this help message
   --project, -P         PROJECT = GNAT project file (.gpr) (mandatory!)
   --summary, -s         List summary (per file)
   --report-mode, -r     Output reporting mode (REPORT-MODE: a = all, f =
                         failed, u = unproved, j = unjustified)
   --sort-by, -c         Sorting criterion (SORT-BY: a = alphabetical, t = by
                         time)
   --cut-off, -p         Cut off point, do not show entities with proof times
                         less than that (CUT-OFF: <numeral>[s|ms])
   --details, -d         Show details for entities (report mode)
   --version, -V         Show version information and exit
   --verbose, -v         Verbose (tracing) output
```

The `--project` argument is the only argument that is not optional, but without
a `--report-mode`, or `--summary` argument, `run_spat` will not output
anything.  It will still try to parse the files it finds, though.

### The `--summary` option

This option is intended to show a quick summary of the files analyzed.

```sh
run_spat -s -P saatana.gpr
```
Typical output would look like this:

```
saatana-crypto.spark        => (Flow  => 9.0 ms,
                                Proof => 6.8 s)
saatana-crypto-phelix.spark => (Flow  => 206.5 ms,
                                Proof => 568.7 s)
saatana.spark               => (Flow  => 464.0 µs,
                                Proof => 0.0 s)
saatana-crypto-lemmas.spark => (Flow  => 2.1 ms,
                                Proof => 2.2 s)
test_phelix_api.spark       => (Flow  => 14.4 ms,
                                Proof => 23.1 s)
```

You can use the `--sort-by` option with `--summary`, either for an alphabetical
list or a list sorted by time (descending order, so files with the most time
needed by the provers come first).  By default, no particular order is imposed
on the output.

*Note* that the `--details` option has no effect on the output here, this
option is designed to work with the `--report-mode` option only.

### The `--report-mode` option

This is the main mode the tool is designed to be run in.  It outputs the list
of entities (i.e. Ada language identifiers) it finds in the `.spark` files that
match the given filter option (see below).

By default, the output has no particular order, but as mentioned in the
previous chapter, with the `--sort-by` option you can force one.

Output can be filtered progressively by applying more restrictions.  These will
be explained below.

If you just want to take a look at how the output of the tool looks like with all
kind of different options, you can take a peek at the repository's
[test directory](https://github.com/HeisenbugLtd/spat/tree/master/test) where I
am storing templates for regression testing.

#### The `--report-mode=all` option

This reports all entities the tool found in the `.spark` files.

Run the command:

```sh
run_spat -ra -P saatana.gpr
```

Typical output looks like this:

```
Saatana.Crypto.Phelix.Encrypt_Bytes        => 174.3 s/189.0 s
Saatana.Crypto.Phelix.Setup_Key            => 206.4 s/219.2 s
Saatana.Crypto.Phelix.Ctx_AAD_Len          => 0.0 s/0.0 s
Saatana.Crypto.Phelix.Encrypt_Packet       => 100.0 ms/2.0 s
Saatana.Crypto.Phelix.MAC_Size_32Predicate => 30.0 ms/30.0 ms
Saatana.Crypto.Phelix.Ctx_Msg_Len          => 0.0 s/0.0 s
Saatana.Crypto.Phelix.Ctx_I                => 0.0 s/0.0 s
Saatana.Crypto                             => 0.0 s/0.0 s
Saatana.Crypto.Phelix.Setup_Key_Called     => 0.0 s/0.0 s
Saatana.Crypto.Phelix.Ctx_Mac_Size         => 0.0 s/0.0 s
Saatana.Crypto.Phelix.Exclusive_Or         => 110.0 ms/540.0 ms
...
```

The first value you see after the Ada entity is the longest proof time, the
second value is the total sum of all proof times for this entity.

#### The `--report-mode=failed` option

When the `--report-mode` is invoked with the `failed` option, it will only show
proof attempts where at least one prover failed to prove the verification
condition.  This does not necessarily mean that the proof itself failed.
Especially, if a different prover could prove the condition later, this is a
good indicator to look if the call order of the provers should be changed.

Example:

```sh
run_spat -ct -rf -P saatana.gpr
```

Typical output:

```
Saatana.Crypto.Phelix.Setup_Key            => 206.4 s/219.2 s
```

Here, we can see that there is one entity where a prover failed to prove the
verification condition.

#### The `--report-mode=unproved` option

When the `--report-mode` is invoked with the `--unproved` option, the tool will
only show proof attempts where ***all*** provers failed to prove the
verification condition, in other words, the tools lists all unproved VCs.

Example:

```sh
run_spat -ct -ru -P sparknacl.gpr
```

Typical output:

```
SPARKNaCl.Sign.Sign                                       => 57.6 s/489.2 s
SPARKNaCl.Car.Nearlynormal_To_Normal                      => 1.4 s/17.5 s
SPARKNaCl.ASR_16                                          => 5.7 s/5.9 s
SPARKNaCl.ASR_8                                           => 3.3 s/3.5 s
SPARKNaCl.ASR_4                                           => 1.2 s/1.4 s
```

Here, we can see that there are five entities with unproven verification
conditions.

#### The `--report-mode=unjustified` option

When the `--report-mode` is invoked with the `--unjustified` option, it will
only show unproven VCs (see above) which are not manually justified (i.e. those
which don't have a justification message).

Example:

```sh
run_spat -ct -rj -P sparknacl.gpr
```

Typical output:

```
SPARKNaCl.Sign.Sign                                       => 57.6 s/489.2 s
SPARKNaCl.Car.Nearlynormal_To_Normal                      => 1.4 s/17.5 s
```

Here, we can see that out of the five entities listed by the previous tool
invocation with `--report-mode=unproved` only two entities are left which have
unproven VCs with no justification message.

### The `--details` option

When invoked together with one of the `--report-mode` options, it will show all
the individual proof attempts/paths for an entity.

Example (with `--report-mode=failed`):

```sh
run_spat -ct -rf -d -P saatana.gpr
```

Output:

```
Saatana.Crypto.Phelix.Setup_Key            => 206.4 s/219.2 s
`-VC_RANGE_CHECK saatana-crypto-phelix.adb:466:44 => 206.4 s/207.1 s
 `-Z3: 206.4 s (Unknown (unknown))
  -CVC4: 640.0 ms (Valid)
```

Same example as the one a little above, but here you can see the individual
proof results.  It seems that Z3 is not well suited to prove this particular
verification condition, but CVC4 can prove it quite fast.  This is a good
indicator that in that particular case, CVC4 should be called first to
optimize proof times.

Without all the other `--report-mode` options, all proof attempts will be shown
in a similar manner.

Please keep in mind that a single proof may have multiple paths leading to it,
resulting in more than just one proof attempt for a single verification
condition.

Another example (with `--unproved`):

```sh
run_spat -ct -ru -d -P sparknacl.gpr
```

Typical output:

```
SPARKNaCl.Sign.Sign                                       => 57.6 s/489.2 s
`-VC_OVERFLOW_CHECK sparknacl-sign.adb:890:36 => 57.6 s/238.7 s
 `-CVC4: 51.6 s (Unknown (unknown))
  -Z3: 7.9 s (Unknown (unknown))
 `-CVC4: 50.5 s (Unknown (unknown))
  -Z3: 8.8 s (Unknown (unknown))
 `-CVC4: 50.7 s (Unknown (unknown))
  -Z3: 7.2 s (Unknown (unknown))
SPARKNaCl.Car.Nearlynormal_To_Normal                      => 1.4 s/17.5 s
`-VC_LOOP_INVARIANT_PRESERV sparknacl-car.adb:324:13 => 1.4 s/1.9 s
 `-CVC4: 1.4 s (Unknown (unknown))
  -Z3: 590.0 ms (Unknown (unknown))
`-VC_ASSERT sparknacl-car.adb:343:31 => 790.0 ms/1.2 s
 `-Z3: 790.0 ms (Unknown (unknown))
  -CVC4: 410.0 ms (Unknown (unknown))
SPARKNaCl.ASR_16                                          => 5.7 s/5.9 s
`-VC_POSTCONDITION sparknacl.ads:355:35 => 5.7 s/5.8 s
 `-Z3: 5.7 s (Unknown (unknown))
  -CVC4: 80.0 ms (Unknown (unknown))
Justified with: "From definition of arithmetic shift right".
SPARKNaCl.ASR_8                                           => 3.3 s/3.5 s
`-VC_POSTCONDITION sparknacl.ads:367:35 => 3.3 s/3.4 s
 `-Z3: 3.3 s (Unknown (unknown))
  -CVC4: 90.0 ms (Unknown (unknown))
Justified with: "From definition of arithmetic shift right".
SPARKNaCl.ASR_4                                           => 1.2 s/1.4 s
`-VC_POSTCONDITION sparknacl.ads:379:35 => 1.2 s/1.3 s
 `-Z3: 1.2 s (Unknown (unknown))
  -CVC4: 80.0 ms (Unknown (unknown))
Justified with: "From definition of arithmetic shift right".
```

As above, but here you can see the individual proof results including any
justification messages (if present).

### The `--cut-off` option

This option allows you to prune the output from possibly irrelevant results.
You can give it a time value (either in seconds, which is the default or in
milliseconds which you can indicate by appending `ms` to the number). Rational
numbers are supported.

If you don't specify a cut off point, the default is `0.0`, in other words, no
cut off.

Please note that due to how `spat` works, the semantics of this cut off point
is different for the `--report-mode` and `--summary` output.

* For `--report-mode` the value applies to all entities and displayed
  verification conditions (for `--details`).  Here the *maximum proof* time
  (i.e. longest time for a single proof) is taken into account, not the
  *total proof* time.  The rationale behind that is that if you want to
  optimize proof times, you need to know which proofs take longest, not how
  many proofs are for a single entity, so I am assuming you are not interested
  in proofs that take less than the cut off point, even if thousands of them
  would add up to a total time well beyond the cut-off point.

  Example:

  `run_spat -ra -ct -P saatana.gpr -p 400ms`

  ```sh
  Saatana.Crypto.Phelix.Setup_Key            => 206.4 s/219.2 s
  Saatana.Crypto.Phelix.Encrypt_Bytes        => 174.3 s/189.0 s
  Saatana.Crypto.Phelix.Decrypt_Bytes        => 4.0 s/18.6 s
  Saatana.Crypto.Phelix.Finalize             => 2.2 s/7.7 s
  Saatana.Crypto.Phelix.H                    => 6.0 s/6.0 s
  ```
  vs.
  
  `run_spat -ra -ct -P saatana.gpr -p 5000ms` (or `-p 5s`, or even `-p 5`)

  ```sh
  Saatana.Crypto.Phelix.Setup_Key            => 206.4 s/219.2 s
  Saatana.Crypto.Phelix.Encrypt_Bytes        => 174.3 s/189.0 s
  Saatana.Crypto.Phelix.H                    => 6.0 s/6.0 s
  ```

  Notice that omitted entries disappear from the middle of the list, because
  the sorting criterion uses the *total time* spent, while the cut off point
  uses the *maximum time*.

  Note that this works the same way if the `--details` option is given.  All
  verification conditions with a proof time below the cut off point will be
  omitted from the report.

* For the `--summary` option the value applies to the *total proof* time
  reported for that file, and *not* for individual proof time like in the
  `--report-mode` option.  That is because the tool assumes that if you want to
  see the summary on a per file (i.e. Ada `package`) basis, you are more
  interested in the total time spent for a file than a single proof.

### The `--verbose` option

This option is mainly used for debugging, it enables extra output about what
`run_spat` is doing (i.e. files found in the given project file, parse results
and some timings).

### The `--version` option

Show version and compiler information for the executable.  If that option is
encountered, no other options take effect and the program immediately exits.

## Tool Limitations

* `spat` only reports accurate timings if it is used after a
  [pristine run of](https://github.com/HeisenbugLtd/spat/issues/22#issue-637738483)
  `gnatprove`.

  That means, let's say you have a failed proof, you change the code, and run
  `gnatprove` again, all times reported for unchanged entitites will be
  `0.0 s`.

  *This can be used as an advantage though:* Let's assume you are trying to
  improve the proof time for a certain proof, so you change the code (like
  adding helping assertion or restructure the logic) and then run `gnatprove`
  again. If you now run `spat` again all unchanged proofs will be reported as
  having a time of 0.0 s, but the verification conditions that had to be
  re-verified will show the time spent proofing them. Which, in the case of
  trying to optimize proof times is exactly what you want.
