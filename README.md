![](https://raw.githubusercontent.com/HeisenbugLtd/web-resources/master/assets/img/logo-spat.png)

# S.P.A.T.
## SPARK Proof Analysis Tool

### Introduction

The SPARK tools (i.e. GNATprove) leave behing a trove of information after a proof run.
This tool is intended to take these and extract some useful information out of it (like
for example, where the provers spent their time, which provers solved the problem first
etc. pp.).

### Motivation

The idea is that making use of that information will help identify and fix bottlenecks
during proof. As the format of these files is virtually undocumented, a little bit of
reverse engineering may be required, but on the other hand, maybe the result is actual
documentation.
