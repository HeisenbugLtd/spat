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

## Terminology

If the below text speaks of arrays and objects, that usually refers to
JSON arrays (i.e. a list) and JSON objects (i.e. name-value pairs).

## The File Format

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

`spark` ::= <"spark" : [ { `name`, `sloc`, `spark` } ]>

`name` ::= <"name" : `json-string`>

`sloc` ::= <"sloc" : [ { `file`, `line` } ]>

`file` ::= <"file" : `json-string`>

`line` ::= <"line" : `json-int`>

#### The `spark[].name` object

Contains the Ada name of the subunit being analyzed.

* Example:
```json
"name": "Saatana.Crypto"
```

#### The `spark[].sloc[]` array

`spark[].sloc` contains JSON objects with source location info. Each
element contains a `file` and a `line` object, containing the source
file name (without any path) and the line within that file.

* Example:
```json
"sloc": [
  {
    "file": "saatana-crypto-lemmas.ads",
    "line": 10
    }
 ]
 ```

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

`flow` ::= <"flow" : [ { `file`, `line`, `col`, `rule`, `severity`,
                         `entity`, `check-tree`, `how-proved` } ]>

`file` ::= <"file" : `json-string`>

`line` ::= <"line" : `json-int`>

`col` ::= <"col" : `json-int`>

`rule` ::= <"rule" : `json-string`> # see: `gnatprove --list-categories`

`severity` ::= <"severity" : `json-string`> # info, warning, error, ...(?)

`entity` ::= <"entity" : `entity-location`>

`entity-location` ::= { `name`, `source-location` }

`name` ::= <"name" : `json-string`>

`source-location` ::= <"sloc" : [ { `file`, `line` } ]>

`file` ::= <"file" : `json-string`>

`line` ::= <"line" : `json-int`>

`check-tree` ::= <"check_tree" : [ { `???` } ]>

`how-proved` ::= <"how_proved" : `json-string`>

#### The `flow[].file` object

Contains the name of the file (again, without path), where the checked
rule applies to.

* Example:
```json
"file": "saatana-crypto.adb"
```

#### The `flow[].line` object

Contains the line number, where the checked rule applies to.

* Example:
```json
"line": 22
```

#### The `flow[].col` object

Contains the column number, where the checked rule applies to.

* Example:
```json
"col": 9
```

#### The `flow[].rule` object

Contains the identifier of the rule being checked.

* Example:
```json
"rule": "DEPENDS_WRONG"
```

Please note that you can get a list of these rules by calling gnatprove
with the switch `--list-categories`, so I am not going to list them
here.

#### The `flow[].severity` object

Contains the severeness of the proof result. As far as I have figured
out, `info` means no error, while `warning` and `error` have their usual
meaning. Other values than these three can possibly occur.

* Example:
```json
"severity": "info"
```

#### The `flow[].entity` object

Contains the objects for `name` of the source file and the `location`
withing that source file of the (enclosing) compilation unit.

* Example:
```json
"entity": {
  "name": "Saatana.Crypto.Oadd.Add_Carry",
  "sloc": # [...]
```

#### The `flow[].entity.name` object

Contains the name of the entity to which the VC applies.

* Example:
```json
"name": "Saatana.Crypto.Oadd.Add_Carry"
```

#### The `flow[].entity.sloc` array

Each element contains the objects `file` and `line` containing the
location of the definition of the entity (I presume).

* Example:
```json
"sloc": [
          {
            "file": "saatana-crypto.adb",
            "line": 18
          }
        ]
```

#### The `flow[].check-tree` object

???

I am assuming this will always be empty for a flow analysis and only be
interesting in the proof part (see below).

#### The `flow[].how-proved` object

Contains the way the VC was dicharged. Unsure which values this object
can have. So far I encountered only `flow` (which makes sense in a flow
analysis step).

* Example:
```json
"how_proved": "flow"
```

### The `proof` array

Similar to the `flow` array this contains results from the proof, for
each VC checked.

The following objects have been seen in the wild:

* file
* line
* col
* rule
* severity
* entity
* check_tree
* how_proved
* check_file
* check_line
* check_col
* how_proved
* stats

#### Grammar Summary

`proof` ::= <"proof" : [ { `file`, `line`, `col`, `rule`, `severity`,
                           `entity`, `check-tree`, `how-proved`,
                           `check-file`, `check-line`, `check-col`,
                           `how-proved`, `stats` } ]>

For the `file`, `line`, `col`, `rule`, `severity`, and `entity` objects
see the flow array section above, they share the same grammar, it seems.

(Actually, I think flow and proof are virtually identical, they just use
different parts of the same structure.)

`check-tree` ::= <"check_tree" : [ { `proof-attempts`,
                                     `transformations` } ]>

`proof-attempts` ::= { `prover` }

`transformations` ::= { `???` }

`prover` ::= <`json-string` : { `result`, `steps`, `time` }>

`result` ::= <"result" : `json-string`>

`steps` ::= <"steps" : `json-int`>

`time` ::= <"time" : `json-float`>

#### The `flow[].check-tree` array

This array contains a list of objects which are further subdivided into
`proof_attempts` and `transformations` object.

##### The `flow[].check-tree[].proof-attempts` object

`proof-attempts` contains objects which are denoted with the prover
name. Weirdly, this is not expressed as a JSON array, but as an object
containing an unspecified number of other JSON objects.

* Example:
```json
"proof_attempts": {
  "CVC4": { # ...
          }
}
```

###### The `flow[].check-tree[].proof-attempts.*` object

Each object contained in the `proof-attempts` object contains the result
of running a specific prover and the object is named after the prover.

* Example:
```json
"CVC4": {
  "result": "Unknown (unknown)",
  "steps": 4602537,
  "time": 1.25920000000000E+02
}
```

As far as I have seen so far, `result` will be "Valid" if the proof was
successful. `steps` holds the number of proof steps the prover has done
(what these steps mean may depend on the prover used), and `time` is
obviously the (wall clock) time the prover spend doing all this.

###### The `flow[].check-tree[].transformations` object

??? Unclear. So far I have only seen empty objects.

### The `assumptions` array

Contains information about the assumptions made in the proof. In other
words, these were not proved themselves, these are conditions that must
hold true if the proof is to be trusted.

#### Grammar Summary

`all-assumptions` ::= <"assumptions" : { [ `assumptions` ], `claim` }>

`assumptions` ::= <"assumptions" : [ `predicate-info` ]>

`claim` ::= <"claim" : { `predicate-info` }>

`predicate-info` ::= { `predicate`, `arg` }

`predicate` ::= <"predicate" : `json-string`>

`arg` ::= <"arg" : { `name`, `sloc` }>

`sloc` ::= <"sloc" : [ { `file`, `line` } ]>

`file` ::= <"file" : `json-string`>

`line` ::= <"line" : `json-int`>

##### The ```all-assumptions``` array

Contains a list of `assumptions` and a `claims` objects. I am assuming
that this lists all assumptions that must hold true for the claim to be
true.

###### The ```all-assumptions[].assumptions[]``` array

Contains `predicate-info` objects.

###### The ```all-assumptions[].claim``` object.

Contains a `claim` object which in turn holds a `predicate-info` object.

###### ```predicate-info``` objects

`predicate-info` objects hold a `predicate` object and an `arg` object.

* Example:
```json
{
  "predicate": "CLAIM_POST",
  "arg": {
    "name": "Saatana.Crypto.To_Stream",
    "sloc": [
      {
        "file": "saatana-crypto.ads",
        "line": 55
      }
    ]
  }
}
```

###### ```predicate``` objects

Holds the identifier of a predicate. So far I've encountered:

* CLAIM_POST (presumably a post condition which must hold true)
* CLAIM_EFFECTS
* CLAIM_AORTE (Absence Of Run Time Errors)

###### ```arg``` objects

Contain a ```name``` object denoting the name of the subunit concerned,
and a ```sloc``` object which references the file and line of its
declaration.

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
