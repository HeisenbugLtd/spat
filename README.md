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

## Invoking the tool

The tool is designed to be run against a directory containing the files left
behind by a run of `gnatprove`. It can handle single files, though, too.

### Command line

Quick help:

`run_spat -h`

will give you a quick overview over the available command line options:

```
usage: run_spat [--help|-h] [--list|-l] [--summary|-s] [--failed-only|-f] 
               [--details|-d] [--sort-by|-c SORT-BY] [--verbose|-v] directory 
               [directory ...] 

Parses .spark files and outputs information about them.

positional arguments:
   directory             directory to look for .spark files in
   
optional arguments:
   --help, -h            Show this help message
   --list, -l            List entities
   --summary, -s         List summary (per file)
   --failed-only, -f     Show only failed attempts
   --details, -d         Show details for entities
   --sort-by, -c         Sort output (SORT-BY: a = alphabetical, t = by time)
   --verbose, -v         Verbose (tracing) output
```

The directory argument is the only argument that is not optional, but without a
`--list`, or `--summary` argument, `run_spat` will not output anything useful.
It will still try to parse the files it finds, though.

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

*Note* that currently the `--details` and `--failed` options have no effect on
the output here, these options are designed to work with the `--list` option
only.

### The `--list` option

This is the main mode the tool is designed to be run in. It outputs the list of
entities (i.e. Ada language identifiers) it finds in the `.spark` files. By
default, the output has no particular order, but as mentioned in the previous
chapter, with the `--sort-by` option you can force one.

Typical output looks like this:

```sh
run_spat -ct -l _build/
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
run_spat -ct -l -f _build/
Saatana.Crypto.Phelix.H                    => 303.8 s/304.3 s
Saatana.Crypto.Phelix.Setup_Key            => 130.4 s/131.2 s
```

Here, we can see that there are two entities where a prover failed to prove the
verification condition. So let's look into this in more detail:

#### The `--details` option

When invoked together with the `--list` option, it will show all the individual
proof attempts/paths for an entity.

Example:

```sh
run_spat -ct -l -d -f _build/
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

Without the `--failed-only` option, all proof attempts will be shown in a similar
manner.

Please keep in mind that a single proof may have multiple paths leading to it,
resulting in more than just one proof attempt for a single verification
condition.

#### The `--verbose` option

This option is mainly used for debugging, it enables extra output about what
`run_spat` is doing (i.e. files found in the given directories, parse results
and timings).
