![](https://raw.githubusercontent.com/HeisenbugLtd/heisenbugltd.github.io/master/assets/img/spat/cover.png)

# SPARK Proof Analysis Tool

There's a chat now in case you have questions or suggestions: [![Join the chat at https://gitter.im/HeisenbugLtd/spat-discussion](https://badges.gitter.im/HeisenbugLtd/spat-discussion.svg)](https://gitter.im/HeisenbugLtd/spat-discussion?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
(This might be a bit more lightweight than opening an issue and we can more
easily discuss features.)

[![Build Linux](https://github.com/HeisenbugLtd/spat/workflows/Build%20Linux/badge.svg)](https://github.com/HeisenbugLtd/spat/actions?query=workflow%3A%22Build+Linux%22)
[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/spat.json)](https://alire.ada.dev/crates/spat.html)

## Content
* ### 1. [Introduction](README.md#introduction)
* ### 2. [Motivation](README.md#motivation)
* ### 3. [Compiling the Tool](README.md#compiling-the-tool)
  * #### 3.1 [Requirements](README.md#requirements)
  * #### 3.2 [Step by Step Instructions](README.md#step-by-step-instructions)
  * #### 3.3 [The `spat.py` plug-in](README.md#the-spatpy-plug-in)
* ### 4. [Invoking the Tool](README.md#invoking-the-tool)
  * #### 4.1 [Command Line](README.md#command-line)
  * #### 4.2 [The `--summary` option](README.md#the---summary-option)
  * #### 4.3 [The `--report-mode` option](README.md#the---report-mode-option)
  * #### 4.4 [The `--details` option](README.md#the---details-option)
  * #### 4.5 [The `--entity` option](README.md#the---entity-option)
  * #### 4.6 [The `--cut-off` option](README.md#the---cut-off-option)
  * #### 4.7 [The `--suggest` option](README.md#the---suggest-option)
  * ##### 4.7.1 [Assumptions](README.md#assumpptions)
  * ##### 4.7.2 [How does it Work?](README.md#how-does-it-work)
  * #### 4.8 [The `--verbose` option](README.md#the---verbose-option)
  * #### 4.9 [The `--raw` option](README.md#the---raw-option)
  * #### 4.10 [The `--version` option](README.md#the---version-option)
* ### 5. [Some Notes on Sorting](README.md#some-notes-on-sorting)
* ### 6. [Tool Limitations](README.md#tool-limitations)

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
to be installed. Version 0.1.1 is recommended, because older version did not
compile with some GNAT FSF versions.  The compiler got stuck and never
finished.

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

### The `spat.py` plug-in

I added a tiny plug-in for `GNAT Studio` that parses the output of `spat` and
shows the proofs with their respective maximum times in the location window.
To make use of the script, you need to link or copy it into your
`~/.gnatstudio/plug-ins` directory.

The plug-in adds the new menu item `SPAT` into the `SPARK` menu in
`GNAT Studio` with the two entries `Show All` and `Show Unproved`.

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
               [--report-mode|-r REPORT-MODE] [--suggest|-g] [--entity|-e 
               ENTITY[ENTITY...]] [--sort-by|-c SORT-BY] [--cut-off|-p CUT-OFF] 
               [--details|-d DETAILS] [--version|-V] [--raw|-R] [--verbose|-v] 

Parses .spark files and outputs information about them.

positional arguments:
   
optional arguments:
   --help, -h            Show this help message
   --project, -P         PROJECT = GNAT project file (.gpr) (mandatory!)
   --summary, -s         List summary (per file)
   --report-mode, -r     Output reporting mode (REPORT-MODE: a = all, f = 
                         failed, u = unproved, j = unjustified [implies 
                         unproved])
   --suggest, -g         Show suggestion for an optimal prover configuration
   --entity, -e          Filter output by ENTITY (regular expression), this 
                         option can  be specified multiple times
   --sort-by, -c         Sorting criterion (SORT-BY: a = alphabetical, s = by 
                         minimum time for successful proof, t = by maximum proof
                          time, p = by minimum steps for successful proof, q = 
                         by maximum steps)
   --cut-off, -p         Cut off point, do not show entities with proof times 
                         less than that (CUT-OFF: <numeral>[s|ms])
   --details, -d         Show details for entities (report mode) (DETAILS: 
                         [1|2|f] for level 1, 2 and full details. Please note 
                         that 2 and f are currently equivalent.)
   --version, -V         Show version information and exit
   --raw, -R             Output timings in raw format (for script use)
   --verbose, -v         Verbose (tracing) output
```

The `--project` argument is the only argument that is not optional, but without
a `--report-mode`, or `--summary`, or `--suggest` argument, `run_spat` will not
output anything.  It will still try to parse the files it finds, though.

### The `--summary` option

This option is intended to show a quick summary of the files analyzed.

```sh
run_spat -s -P saatana.gpr
```
Typical output would look like this:

```
saatana-crypto.spark                     => (Flow  => 9.0 ms,
                                             Proof => 80.0 ms (1 step)/80.0 ms (1 step)/6.8 s)
test_phelix.spark                        => (Flow  => 180.0 µs,
                                             Proof => 0.0 s (0 steps)/0.0 s (0 steps)/0.0 s)
saatana-crypto-phelix.spark              => (Flow  => 206.5 ms,
                                             Proof => 174.3 s (14009 steps)/206.4 s (131078 steps)/568.7 s)
saatana.spark                            => (Flow  => 464.0 µs,
                                             Proof => 0.0 s (0 steps)/0.0 s (0 steps)/0.0 s)
saatana-crypto-lemmas.spark              => (Flow  => 2.1 ms,
                                             Proof => 210.0 ms (1 step)/210.0 ms (1 step)/2.2 s)
test_phelix_api.spark                    => (Flow  => 14.4 ms,
                                             Proof => 240.0 ms (1 step)/240.0 ms (1 step)/23.1 s)
saatana-crypto-stream_tools.spark        => (Flow  => 71.0 µs,
                                             Proof => 0.0 s (0 steps)/0.0 s (0 steps)/0.0 s)
saatana-crypto-phelix-test_vectors.spark => (Flow  => 24.0 µs,
                                             Proof => 0.0 s (0 steps)/0.0 s (0 steps)/0.0 s)
```

You can use the `--sort-by` option with `--summary`, either for an alphabetical
list (`--sort-by=a`), a list sorted by time (`--sort-by=t`, descending order,
so files with the most time needed by the provers come first), or a list sorted
by steps (`--sort-by=p`).  The options to sort by maximum time for successful
proof (`--sort-by=s`) or by maximum steps for successful proof (`--sort-by=q`)
are also available.  By default, no particular order is imposed on the output.

For the meaning of the three timings after `Proof =>`, please see below.

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

If you just want to take a look at how the output of the tool looks like with
all kind of different options, you can take a peek at the repository's
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
Saatana.Crypto.Phelix.Encrypt_Bytes        => 174.3 s (10170 steps)/174.3 s (10170 steps)/189.0 s
Saatana.Crypto.Phelix.Setup_Key            => 640.0 ms (221 steps)/206.4 s (131078 steps)/219.2 s
Saatana.Crypto.Phelix.Ctx_AAD_Len          => 0.0 s (0 steps)/0.0 s (0 steps)/0.0 s
Saatana.Crypto.Phelix.Encrypt_Packet       => 100.0 ms (1 step)/100.0 ms (1 step)/2.0 s
Saatana.Crypto.Phelix.MAC_Size_32Predicate => 30.0 ms (1 step)/30.0 ms (1 step)/30.0 ms
Saatana.Crypto.Phelix.Ctx_Msg_Len          => 0.0 s (0 steps)/0.0 s (0 steps)/0.0 s
Saatana.Crypto.Phelix.Ctx_I                => 0.0 s (0 steps)/0.0 s (0 steps)/0.0 s
Saatana.Crypto                             => 0.0 s (0 steps)/0.0 s (0 steps)/0.0 s
Saatana.Crypto.Phelix.Setup_Key_Called     => 0.0 s (0 steps)/0.0 s (0 steps)/0.0 s
Saatana.Crypto.Phelix.Ctx_Mac_Size         => 0.0 s (0 steps)/0.0 s (0 steps)/0.0 s
Saatana.Crypto.Phelix.Exclusive_Or         => 110.0 ms (1 step)/110.0 ms (1 step)/540.0 ms
...
```

The first value you see after the Ada entity is the longest time (and steps)
needed for a single successful proof, the second value is the maximum time (and
steps) needed for a proof (successful or not), and the third value is the total
sum of all proof times for this entity.

If the first and the second value vastly differ, that usually means that one of
the provers involved could not prove a certain item, but another one could and
was better at it. See below how to analyze this in a more detailed way.

If the first value is shown as `--`, then that means, there was at least one
unsuccessful proof for this entity.  An example:

```
...
SPARKNaCl.MAC.Onetimeauth                                 => 340.0 ms (1 step)/340.0 ms (1 step)/3.7 s
SPARKNaCl.Seminormal_GFPredicate                          => 0.0 s (0 steps)/0.0 s (0 steps)/0.0 s
SPARKNaCl.ASR_4                                           => --/1.2 s (14001 steps)/1.4 s
SPARKNaCl.Car.Nearlynormal_To_Normal                      => --/1.4 s (803 steps)/17.5 s
SPARKNaCl.ASR_8                                           => --/3.3 s (14001 steps)/3.5 s
SPARKNaCl.Sign.Unpackneg.Pow_2523                         => 0.0 s (0 steps)/0.0 s (0 steps)/0.0 s
...
```

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
Saatana.Crypto.Phelix.Setup_Key => 640.0 ms (221 steps)/206.4 s (131078 steps)/219.2 s
```

Here, we can see that there is one entity where a prover failed to prove the
verification condition.  As mentioned above, here you can see that the time for
*longest successful proof* (640 ms) greatly differs from the
*maximum time for a single proof* (206 s).  This is a clear indicator, that one
of the provers is not well suited to prove a certain verification condition.

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
SPARKNaCl.Sign.Sign                  => --/57.6 s (14007 steps)/489.2 s
SPARKNaCl.Car.Nearlynormal_To_Normal => --/1.4 s (803 steps)/17.5 s
SPARKNaCl.ASR_16                     => --/5.7 s (14001 steps)/5.9 s
SPARKNaCl.ASR_8                      => --/3.3 s (14001 steps)/3.5 s
SPARKNaCl.ASR_4                      => --/1.2 s (14001 steps)/1.4 s
```

Here, we can see that there are five entities with unproven verification
conditions (and no reported maximum time for successful proof, of course, so it
is shown as `--`).

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
SPARKNaCl.Sign.Sign                  => --/57.6 s (14007 steps)/489.2 s
SPARKNaCl.Car.Nearlynormal_To_Normal => --/1.4 s (803 steps)/17.5 s
```

Here, we can see that out of the five entities listed by the previous tool
invocation with `--report-mode=unproved` only two entities are left which have
unproven VCs with no justification message.

### The `--details` option

When invoked together with one of the `--report-mode` options, it will show all
the individual proof attempts (level 1) and paths (level 2) for an entity.

Example (with `--report-mode=failed` and detail level 1):

```sh
run_spat -ct -rf -d 1 -P saatana.gpr
```

Output:

```
Saatana.Crypto.Phelix.Setup_Key                   =>                                                       640.0 ms (221 steps)/206.4 s (131078 steps)/219.2 s
`-VC_RANGE_CHECK saatana-crypto-phelix.adb:466:44 => 640.0 ms (221 steps)/206.4 s (131078 steps)/207.1 s
```

Example (with `--report-mode=failed`):

```sh
run_spat -ct -rf -d -P saatana.gpr
```

Output:

```
Saatana.Crypto.Phelix.Setup_Key                   =>                                                       640.0 ms (221 steps)/206.4 s (131078 steps)/219.2 s
`-VC_RANGE_CHECK saatana-crypto-phelix.adb:466:44 => 640.0 ms (221 steps)/206.4 s (131078 steps)/207.1 s
 `-Z3: 206.4 s (131078 steps), Unknown (unknown)
  -CVC4: 640.0 ms (221 steps), Valid
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
SPARKNaCl.Sign.Sign                                  =>                                  --/57.6 s (14007 steps)/489.2 s
`-VC_OVERFLOW_CHECK sparknacl-sign.adb:890:36        => --/57.6 s (14007 steps)/238.7 s
 `-CVC4: 51.6 s (14001 steps), Unknown (unknown)
  -Z3: 7.9 s (14006 steps), Unknown (unknown)
 `-CVC4: 50.5 s (14001 steps), Unknown (unknown)
  -Z3: 8.8 s (14006 steps), Unknown (unknown)
 `-CVC4: 50.7 s (14001 steps), Unknown (unknown)
  -Z3: 7.2 s (14007 steps), Unknown (unknown)
SPARKNaCl.Car.Nearlynormal_To_Normal                 =>                                  --/1.4 s (803 steps)/17.5 s
`-VC_LOOP_INVARIANT_PRESERV sparknacl-car.adb:324:13 => --/1.4 s (367 steps)/1.9 s
 `-CVC4: 1.4 s (1 step), Unknown (unknown)
  -Z3: 590.0 ms (367 steps), Unknown (unknown)
`-VC_ASSERT sparknacl-car.adb:343:31                 => --/790.0 ms (635 steps)/1.2 s
 `-Z3: 790.0 ms (635 steps), Unknown (unknown)
  -CVC4: 410.0 ms (1 step), Unknown (unknown)
SPARKNaCl.ASR_16                                     =>                                  --/5.7 s (14001 steps)/5.9 s
`-VC_POSTCONDITION sparknacl.ads:355:35              => --/5.7 s (14001 steps)/5.8 s
 `-Z3: 5.7 s (14001 steps), Unknown (unknown)
  -CVC4: 80.0 ms (1 step), Unknown (unknown)
Justified with: "From definition of arithmetic shift right".
SPARKNaCl.ASR_8                                      =>                                  --/3.3 s (14001 steps)/3.5 s
`-VC_POSTCONDITION sparknacl.ads:367:35              => --/3.3 s (14001 steps)/3.4 s
 `-Z3: 3.3 s (14001 steps), Unknown (unknown)
  -CVC4: 90.0 ms (1 step), Unknown (unknown)
Justified with: "From definition of arithmetic shift right".
SPARKNaCl.ASR_4                                      =>                                  --/1.2 s (14001 steps)/1.4 s
`-VC_POSTCONDITION sparknacl.ads:379:35              => --/1.2 s (14001 steps)/1.3 s
 `-Z3: 1.2 s (14001 steps), Unknown (unknown)
  -CVC4: 80.0 ms (1 step), Unknown (unknown)
Justified with: "From definition of arithmetic shift right".
```

As above, but here you can see the individual proof results including any
justification messages (if present).

### The `--entity` option

Sometimes the (detailed) output is just too much and if you want to only see
the results for certain entities, then the `--entity` option is for you.  You
can specify multiple `--entity` options.  When invoked together with one of the
`--report-mode` options, it will show only those entities that match one of the
given filters.

The expression after `--entity` is expected to be a valid regular expression.
That means, in most cases when you do not want to specify the fully qualified
name you should start the expression with a "match anything" `.*`.

Example (with `--unproved`), show only those entities matching "ASR"
(Arithmetic Shift Right):

```sh
run_spat -ct -ru -d -e ".*ASR.*" -P sparknacl.gpr
```

This shows all unproved entities that match the expression `".*ASR.*"`:

```
SPARKNaCl.ASR_16                        =>                               --/5.7 s (14001 steps)/5.9 s
`-VC_POSTCONDITION sparknacl.ads:355:35 => --/5.7 s (14001 steps)/5.8 s
 `-Z3: 5.7 s (14001 steps), Unknown (unknown)
  -CVC4: 80.0 ms (1 step), Unknown (unknown)
Justified with: "From definition of arithmetic shift right".
SPARKNaCl.ASR_8                         =>                               --/3.3 s (14001 steps)/3.5 s
`-VC_POSTCONDITION sparknacl.ads:367:35 => --/3.3 s (14001 steps)/3.4 s
 `-Z3: 3.3 s (14001 steps), Unknown (unknown)
  -CVC4: 90.0 ms (1 step), Unknown (unknown)
Justified with: "From definition of arithmetic shift right".
SPARKNaCl.ASR_4                         =>                               --/1.2 s (14001 steps)/1.4 s
`-VC_POSTCONDITION sparknacl.ads:379:35 => --/1.2 s (14001 steps)/1.3 s
 `-Z3: 1.2 s (14001 steps), Unknown (unknown)
  -CVC4: 80.0 ms (1 step), Unknown (unknown)
Justified with: "From definition of arithmetic shift right".
```

As above, but the `SPARKNaCl.Sign.Sign` and
`SPARKNaCl.Car.Nearlynormal_To_Normal` have been omitted as they don't match
the given filter expression.

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
  verification conditions (for `--details` with at least level 1).  Here the
  *maximum proof* time (i.e. longest time for a single proof) is taken into
  account, not the *total proof* time.  The rationale behind that is that if
  you want to optimize proof times, you need to know which proofs take longest,
  not how many proofs are for a single entity, so I am assuming you are not
  interested in proofs that take less than the cut off point, even if thousands
  of them would add up to a total time well beyond the cut-off point.

  Example:

  `run_spat -ra -ct -p 400ms -P saatana.gpr`

  ```sh
  Saatana.Crypto.Phelix.Setup_Key     => 640.0 ms (221 steps)/206.4 s (131078 steps)/219.2 s
  Saatana.Crypto.Phelix.Encrypt_Bytes => 174.3 s (10170 steps)/174.3 s (10170 steps)/189.0 s
  Saatana.Crypto.Phelix.Decrypt_Bytes => 4.0 s (3331 steps)/4.0 s (3331 steps)/18.6 s
  Saatana.Crypto.Phelix.Finalize      => 2.2 s (3029 steps)/2.2 s (3029 steps)/7.7 s
  Saatana.Crypto.Phelix.H             => 6.0 s (14009 steps)/6.0 s (14009 steps)/6.0 s
  ```

  vs.
  
  `run_spat -ra -ct -P saatana.gpr -p 5000ms` (or `-p 5s`, or even `-p 5`)

  ```sh
  Saatana.Crypto.Phelix.Setup_Key     => 640.0 ms (221 steps)/206.4 s (131078 steps)/219.2 s
  Saatana.Crypto.Phelix.Encrypt_Bytes => 174.3 s (10170 steps)/174.3 s (10170 steps)/189.0 s
  Saatana.Crypto.Phelix.H             => 6.0 s (14009 steps)/6.0 s (14009 steps)/6.0 s
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

### The `--suggest` option

  > *First of all, this option is highly experimental.* At some time in the
  > near future I might write something more extensive about the implementation
  > in the project's Wiki, but for now, the following must suffice.

When `spat` is called with the option, it will try to find a better
configuration, i.e. file specific options for `gnatprove` which you can add to
your project file.

#### Assumptions

* `spat` assumes that the files are fully proven to the extent of the
  capability of the provers.  Some undischarged VCs might remain, but in
  general, the project should be in a good state and the provers should be able
  to prove what can be proved.

#### How Does it Work?

The only information available to `spat` when a prover succeeds is the time and
the steps it took it to succeed.  Only when a prover fails and the next one in
the chain is called, `spat` can infer more information.

For each source file referenced in the parsed `.spark` files, `spat` records
which provers were involved and also the maximum number of steps and the
longest time for each discharged VC (i.e. `spat` assumes that undischarged VCs
are there by design).

That means, the output regarding steps and time (see example below) takes the
*current* state of the project into account.

As for the order of the provers, that is a bit more tricky due to the lack of
information.  Because `spat` cannot know what is not known, I decided to go
with a rather simplistic heuristic which can be boiled down to two points:

> 1. The more time a prover spends in unsuccessful proofs, the less likely it
>    is to succeed.
> 2. The more time a prover spends in successful proofs, the more likely it is
>    to succeed.

This results in a very simple sorting order:  The less "fail" time a prover has,
the better it seems to be, and if that cannot be decided because the accumulated
"fail" time of the provers for the source file is equal, the prover with the
greater "success" time wins.

Of course, in most practical scenarios, the prover that will always be called
first will likely also have the most success time.

Example:

`run_spat -g -P sparknacl.gpr`

```
Warning: You requested a suggested prover configuration.
Warning: This feature is highly experimental.
Warning: Please consult the documentation.

package Prove is
   for Proof_Switches ("sparknacl-car.adb") use ("--prover=Z3,CVC4", "--steps=803", "--timeout=2");
   for Proof_Switches ("sparknacl-core.adb") use ("--prover=CVC4", "--steps=1", "--timeout=1");
   for Proof_Switches ("sparknacl-cryptobox.adb") use ("--prover=CVC4", "--steps=1", "--timeout=1");
   for Proof_Switches ("sparknacl-hashing.adb") use ("--prover=CVC4", "--steps=1", "--timeout=1");
   for Proof_Switches ("sparknacl-mac.adb") use ("--prover=CVC4", "--steps=1", "--timeout=1");
   for Proof_Switches ("sparknacl-scalar.adb") use ("--prover=CVC4", "--steps=1", "--timeout=1");
   for Proof_Switches ("sparknacl-secretbox.adb") use ("--prover=CVC4", "--steps=1", "--timeout=1");
   for Proof_Switches ("sparknacl-sign.adb") use ("--prover=Z3,CVC4", "--steps=14007", "--timeout=10");
   for Proof_Switches ("sparknacl-stream.adb") use ("--prover=CVC4", "--steps=1", "--timeout=1");
   for Proof_Switches ("sparknacl-utils.adb") use ("--prover=Z3,CVC4", "--steps=1536", "--timeout=1");
   for Proof_Switches ("sparknacl.adb") use ("--prover=Z3,CVC4", "--steps=1", "--timeout=1");
end Prove;
```

*Here you may want to add the `--verbose` switch to see some debug output.*

As explained above, the `steps` and `timeout` output should be fairly accurate.
The call order of the provers is at best an educated guess.

Also, please note that this output never lists provers that have never been
called, simply because we know nothing about them.

Worth mentioning is also that the steps reported here are different from the
steps reported in the `--report-mode` option.  This is due to the fact that
within the `.spark` files steps are currently reported differently than the way
`gnatprove` looks at them.  The thing is that each prover has their own notion
of steps, but giving a `--steps` option to `gnatprove` should behave the same
regardless of the prover involved, so `gnatprove` implements some transformation
to scale the number of steps to roughly the equivalent of alt-ergo steps.

I decided to implement the same scaling values that `gnatprove` uses (which are
also the steps which are reported in the `stats` object of the `.spark` files).

### The `--verbose` option

This option is mainly used for debugging, it enables extra output about what
`run_spat` is doing (i.e. files found in the given project file, parse results
and some timings).

### The `--version` option

Show version and compiler information for the executable.  If that option is
encountered, no other options take effect and the program immediately exits.

### The `--raw` option

This is intended for scripts parsing the output of `spat`.  If this switch is
specified times are shown as raw numbers instead of properly scaled, human
readable output.  This should make it easier for scripts to parse the numbers.

## Some Notes on Sorting

The sort option *by successful proof time* (i.e. `--sort-by=s`) is may work in a
slightly counter-intuitive way, so I explain it a bit.  The explanation is
similar for sorting *by successful proof steps* (i.e. `--sort-by=q`).

This option does not apply for the `--cut-off` option, which still only takes
the [maximum proof time into account](https://github.com/HeisenbugLtd/spat/issues/33).

The [idea behind this sorting option](https://github.com/HeisenbugLtd/spat/issues/29)
is that you may want to know where provers spend useless time (e.g. where a
prover is called which is known to not be able to prove a certain verification
condition).  After all, all provers have their strengths and weaknesses.

Sorting by maximum successful proof time usually only makes sense when also
invoked with the `--report-mode=failed` (or even more restrictive) filter
option.  That is because if there are no failed attempts, it doesn't really
matter what the *best* proof time would be, after all, the provers take all the
time they need.  If you want to know the total effort spent, you can just as
well use `--sort-by=t`.

Examples (mostly to show how the difference between `--sort-by=t` and
`--sort-by=s` work):

* By time:

  `run_spat -ct -rf -P sparknacl.gpr`

  ```
  SPARKNaCl.Sign.Sign                   => --/57.6 s (14007 steps)/489.2 s
  SPARKNaCl.Omultiply                   => 700.0 ms (1 step)/19.1 s (14001 steps)/28.6 s
  SPARKNaCl.Car.Nearlynormal_To_Normal  => --/1.4 s (803 steps)/17.5 s
  SPARKNaCl.ASR_16                      => --/5.7 s (14001 steps)/5.9 s
  SPARKNaCl.ASR_8                       => --/3.3 s (14001 steps)/3.5 s
  SPARKNaCl.Utils.Pack_25519.Subtract_P => 150.0 ms (1 step)/180.0 ms (1 step)/1.8 s
  SPARKNaCl.ASR_4                       => --/1.2 s (14001 steps)/1.4 s
  ```

* By successful proof:

  `run_spat -cs -rf -P sparknacl.gpr`

  ```
  SPARKNaCl.Omultiply                   => 700.0 ms (1 step)/19.1 s (14001 steps)/28.6 s
  SPARKNaCl.Utils.Pack_25519.Subtract_P => 150.0 ms (1 step)/180.0 ms (1 step)/1.8 s
  SPARKNaCl.Sign.Sign                   => --/57.6 s (14007 steps)/489.2 s
  SPARKNaCl.Car.Nearlynormal_To_Normal  => --/1.4 s (803 steps)/17.5 s
  SPARKNaCl.ASR_16                      => --/5.7 s (14001 steps)/5.9 s
  SPARKNaCl.ASR_8                       => --/3.3 s (14001 steps)/3.5 s
  SPARKNaCl.ASR_4                       => --/1.2 s (14001 steps)/1.4 s
  ```

Notice, how the entries for `SPARKNaCl.Omultiply` and
`SPARKNaCl.Utils.Pack_25519.Subtract_P` moved up?

Please note that unproved items are still shown, but due to the fact that they
are unproved, they have no successful proof time (although there may be partial
successes for the involved VCs), so with this sorting option they will always
appear at the end.

Looking at `Omultiply` in detail:

`run_spat -d -cs -ra -P sparknacl.gpr -e .*Omultiply`

```
SPARKNaCl.Omultiply                              =>                                                700.0 ms (1 step)/19.1 s (14001 steps)/28.6 s
`-VC_LOOP_INVARIANT_PRESERV sparknacl.adb:164:13 => 40.0 ms (1 step)/19.1 s (14001 steps)/19.1 s
 `-CVC4: 19.1 s (14001 steps), Unknown (unknown)
  -Z3: 40.0 ms (1 step), Valid
`-VC_LOOP_INVARIANT_PRESERV sparknacl.adb:80:16  => 700.0 ms (1 step)/700.0 ms (1 step)/930.0 ms
 `-CVC4: 700.0 ms (1 step), Valid
 `-CVC4: 230.0 ms (1 step), Valid
`-VC_RANGE_CHECK sparknacl.adb:75:36             => 170.0 ms (1 step)/170.0 ms (1 step)/480.0 ms
 `-CVC4: 170.0 ms (1 step), Valid
 `-CVC4: 120.0 ms (1 step), Valid
 `-CVC4: 110.0 ms (1 step), Valid
 `-CVC4: 80.0 ms (1 step), Valid
`-VC_OVERFLOW_CHECK sparknacl.adb:75:48          => 140.0 ms (1 step)/140.0 ms (1 step)/420.0 ms
 `-CVC4: 140.0 ms (1 step), Valid
 `-CVC4: 110.0 ms (1 step), Valid
 `-CVC4: 100.0 ms (1 step), Valid
 `-CVC4: 70.0 ms (1 step), Valid
`-VC_LOOP_INVARIANT_INIT sparknacl.adb:80:16     => 120.0 ms (1 step)/220.0 ms (1 step)/400.0 ms
 `-CVC4: 220.0 ms (1 step), Unknown (unknown)
  -Z3: 60.0 ms (1 step), Valid
 `-CVC4: 120.0 ms (1 step), Valid
...
```

You can see, where the 700 ms for the longest time for a successful proof comes
from (this VC would not be shown at all in `--report-mode=failed`).

## Tool Limitations

* `spat` only reports accurate timings if it is used after a
  [pristine run](https://github.com/HeisenbugLtd/spat/issues/22#issue-637738483)
  of `gnatprove`.

  That means, let's say you have a failed proof, you change the code, and run
  `gnatprove` again, all times reported for unchanged entitites will be
  `0.0 s`.

  *This can be used as an advantage though:* Let's assume you are trying to
  improve the proof time for a certain proof, so you change the code (like
  adding helping assertion or restructure the logic) and then run `gnatprove`
  again. If you now run `spat` again all unchanged proofs will be reported as
  having a time of 0.0 s, but the verification conditions that had to be
  re-verified will show the time spent proving them. Which, in the case of
  trying to optimize proof times is exactly what you want.

* The `--suggest` option is highly experimental.  As the name says, it is a
  suggestion.
