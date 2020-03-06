![](https://raw.githubusercontent.com/HeisenbugLtd/heisenbugltd.github.io/master/assets/img/spat/cover.png)

# SPARK Proof Analysis Tool

## Introduction

The SPARK tools (i.e. GNATprove) leave behind a trove of information after a proof run.
`spat` is intended to take these information and extract some useful information from
it (like for example, where the provers spent their time, which provers solved the
problem first etc. pp.).
In a way, this tool serves a similar purpose as the [gnatprove2xls](https://github.com/damaki/gnatprove2xls)
tool, but it parses the "raw" data instead of post-processed output, hence it has more
information available, and it's written in Ada. (I considered using Python, but rejected
it, because that would have been too easy).

## Motivation

The idea is that making use of that information will help identify and fix bottlenecks
during proof. As the format of these files is virtually undocumented, a little bit of
reverse engineering may be required, but on the other hand, maybe the result is actual
[documentation](https://github.com/HeisenbugLtd/spat/blob/master/doc/spark_file_format.md).
