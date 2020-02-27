# SPARK file format

## Preface

The following documentation is reverse engineered, so information
contained in here should be taken with a grain of salt (or two grains,
and maybe some Habanero sauce).

Some information about the file format has been extracted from the GPS
plugin `spark2014.py`.

## Introduction

The `.spark` files are in [JSON format](https://www.json.org/json-en.html)
and contain information about the attempts to discharge verification
conditions (VCs), the kind of proof attempted (assertion, runtime check,
etc.), which provers have been tried and so on.

From `spark2014.py`:

> The json file, if it exists and is a valid JSON value, is a dict
  with two entries "flow" and "proof" (both entries may be absent).
  Each entry is mapped to a list of dictionaries.

  This is about all the very specific information I could find in there
  and it's most definitely not complete.

## The file format

Each `.spark` file contains a single JSON object (i.e. name-value pairs).

So far I have seen:

* spark
* flow
* proof
* assumptions
* timings

### The `spark` array

Contains JSON objects where each object contains information about the
Ada unit (`package`, `subprogram`, ...) being analyzed: The `name` of
the unit, the source location `sloc` and **presumably** the options used
for the SPARK analysis in another object called `spark`.

#### Grammar Summary

`spark` ::= [ { `name`, `sloc`, `spark` } ]

`name` ::= <"name" : `json-string`>

`sloc` ::= [ { `file`, `line` } ]

`file` ::= <"file" : `json-string`>

`line` ::= <"line" : `json-int`>

#### The `spark[].name` object

Contains the Ada name of the unit being analyzed.

#### The `spark[].sloc[]` array

`spark[].sloc` contains JSON objects with source location info. Each
element contains a `file` and a `line` object, containing the source
file name (without any path) and the line within that file.

As far as I have figured out, this simply points to the line where the
analyzed Ada unit is declared, e.g. the line of an Ada `package`
specification.

### The `flow` array

This contains results from the data flow analysis, for each rule checked.
The following objects have been seen in the wild:

* file
* line
* col
* rule
* severity
* entity
* check_tree
* how_proved

#### Grammar Summary

`flow` ::= [ { `file`, `line`, `col`, `rule`, `severity`,`entity`,
               `check-tree`, `how-proved` } ]

`file` ::= <"file" : json_string>

`line` ::= <"line" : json_int>

`col` ::= <"col" : json_int>

`rule` ::= <"rule" : json_string> # see: `gnatprove --list-categories`

`severity` ::= <"severity" : json_string> # info, warning, error, ...(?)

`entity` ::= <"entity" : `entity-location`>

`entity-location` ::= { `name`, `source-location` }

`name` ::= <"name" : `json_string`>

`source-location` ::= <"sloc" : [ { `file`, `line` } ]>

`file` ::= <"file" : `json_string`>

`line` ::= <"line" : `json_int`>

`check-tree` ::= <"check_tree" : [ { `???` } ]>

`how-proved` ::= <"how_proved" : json_string>

`entity-location` ::= ...

#### The `flow[].file` object

Contains the name of the file (again, without path), where the checked
rule applies to.

#### The `flow[].line` object

Contains the line number, where the checked rule applies to.

#### The `flow[].col` object

Contains the column number, where the checked rule applies to.

#### The `flow[].rule` object

Contains the identifier of the rule being checked.

Please note that you can get a list of these rules by calling gnatprove
with the switch `--list-categories`.

#### The `flow[].severity` object

Contains the severeness of the proof result. As far as I have figured
out, `info` means no error, while `warning` and `error` have their usual
meaning. Other values than these three can possibly occur.

#### The `flow[].entity` object

Contains the objects for `name` of the source file and the `location`
withing that source file of the (enclosing) compilation unit.

#### The `flow[].entity.name` object

Contains the name of the entity to which the VC applies.

#### The `flow[].entity.sloc` array

Each element contains the objects `file` and `line` containing the
location of the definition of the entity (I presume).

#### The `flow[].check_tree` object

???

#### The `flow[].how_proved` object

Contains the way the VC was dicharged. Unsure which values this object
can have. So far I encountered only `flow` (which makes sense in a flow
analysis step).

### The `proof` object

#### Grammar

`proof´ ::= `???`**[]**

### The `assumptions` object

#### Grammar

`assumptions` ::= `???`**[]**

### The `timings` object

Seems to contain global timings from the whole proof (i.e. summed up
execution times etc.).

#### Grammar

`timings` ::= { `marking`, `globals-basic`, `globals-advanced`,
                `flow-analysis`, `codepeer-results`,
                `init-why-sections`, `translation-of-standard`,
                `translation-of-compilation-unit`, `proof` }

`marking` ::= <"marking" : `json-float`>

`globals-basic` ::= <"globals (basic)" : `json-float`>

`globals-advanced` ::= <"globals/properties (advanced)" : `json-float`>

`flow-analysis` ::= <"flow analysis" : `json-float`>

`codepeer-results` ::= <"codepeer results" : `json-float`>

`init-why-sections` ::= <"init_why_sections" : `json-float`>

`translation-of-standard` ::= <"translation of standard" : `json-float`>

`translation-of-compilation-unit` ::= <"translation of compilation unit" : `json-float`>

`proof` ::= <"proof" : `json-float`>
