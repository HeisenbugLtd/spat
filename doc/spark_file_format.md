# SPARK file format

## 0 Preface

The following documentation is reverse engineered, so information contained in
here should be taken with a grain of salt (or two grains, and maybe some
Habanero sauce).

Some information about the file format has been extracted from the GPS plugin
[`spark2014.py`](https://github.com/AdaCore/gps/blob/master/share/plug-ins/spark2014.py).

Also, see this [section of the SPARK 2014 User's Guide](https://github.com/AdaCore/spark2014/blob/master/docs/ug/en/source/how_to_investigate_unproved_checks.rst#looking-at-machine-parsable-gnatprove-output) ([online version](https://docs.adacore.com/spark2014-docs/html/ug/en/source/how_to_investigate_unproved_checks.html#looking-at-machine-parsable-gnatprove-output))
about these files.

## 1 Introduction

The `.spark` files are in [JSON format](https://www.json.org/json-en.html) and
contain information about the attempts to discharge verification conditions
(VCs), the kind of proof attempted (assertion, runtime check, etc.), which
provers have been tried and so on.

From `spark2014.py`:

> The json file, if it exists and is a valid JSON value, is a dict with two
  entries "flow" and "proof" (both entries may be absent). Each entry is mapped
  to a list of dictionaries.

This is about all the very specific information I could find in there and it is
most definitely not complete.

## 2 Terminology

If the below text speaks of arrays and objects, that usually refers to JSON
arrays (i.e. a list) and JSON objects (i.e. name-value pairs).

## 3 The File Format

Each `.spark` file contains a single JSON object (i.e. name-value pairs).

So far I have seen:

* [`spark`](spark_file_format.md#31-the-spark-array)
* [`flow`](spark_file_format.md#32-the-flow-array)
* [`proof`](spark_file_format.md#33-the-proof-array)
* [`assumptions`](spark_file_format.md#34-the-assumptions-array)
* [`timings`](spark_file_format.md#35-the-timings-object)
* **GNAT CE 2020** `session_map`

### 3.1 The `spark` array

Contains JSON objects where each object contains information about the Ada unit
(`package`, `subprogram`, ...) being analyzed: The `name` of the unit, the
source location `sloc` and the options used for the SPARK analysis in another
object called `spark`.

As far as I figured out, this array contains a list of entities referenced in
later sections, i.e. a list of all sub-units analyzed.

#### 3.1.1 Grammar Summary

`spark` ::= "spark" : [ { [`name`](spark_file_format.md#312-the-name-object),
                          [`sloc`](spark_file_format.md#313-the-sloc-array),
                          `spark` } ]

`name` ::= "name" : `json-string`

`sloc` ::= "sloc" : [ { `file`, `line` } ]

`file` ::= "file" : `json-string`

`line` ::= "line" : `json-int`

#### 3.1.2 The `name` object

Contains the Ada name of the subunit being analyzed.

* Example:
```json
"name": "Saatana.Crypto"
```

#### 3.1.3 The `sloc` array

`spark[].sloc` contains JSON objects with source location info. Each element
contains a `file` and a `line` object, containing the source file name (without
any path) and the line within that file.

* Example:
```json
"sloc": [
  {
    "file": "saatana-crypto-lemmas.ads",
    "line": 10
    }
 ]
 ```

As far as I have figured out, this simply points to the line where the analyzed
Ada unit is declared, e.g. the line of an Ada `package` or `subprogram`
declaration.

### 3.2 The `flow` array

This contains results from the data flow analysis, for each rule checked. The
following objects have been seen in the wild:

* [`file`](spark_file_format.md#322-the-file-object)
* [`line`](spark_file_format.md#323-the-line-object)
* [`col`](spark_file_format.md#324-the-col-object)
* [`rule`](spark_file_format.md#325-the-rule-object)
* [`severity`](spark_file_format.md#326-the-severity-object)
* [`entity`](spark_file_format.md#327-the-entity-object)
* [`check_tree`](spark_file_format.md#328-the-check-tree-array)
* [`how_proved`](spark_file_format.md#329-the-how-proved-object)

*Note*: It seems to share the same data structure with the
[`proof`](spark_file_format.md#33-the-proof-array) array.

#### 3.2.1 Grammar Summary

`flow` ::= "flow" : [ { `file`, `line`, `col`, `rule`, `severity`, `entity`,
                        `check-tree`, `how-proved` } ]

`file` ::= "file" : `json-string`

`line` ::= "line" : `json-int`

`col` ::= "col" : `json-int`

`rule` ::= "rule" : `json-string` # see: `gnatprove --list-categories`

`severity` ::= "severity" : `json-string` # info, warning, error, ...(?)

`entity` ::= "entity" : `entity-location`

`entity-location` ::= { `name`, `source-location` }

`name` ::= "name" : `json-string`

`source-location` ::= "sloc" : [ { `file`, `line` } ]

`file` ::= "file" : `json-string`

`line` ::= "line" : `json-int`

`check-tree` ::= "check_tree" : [ { `???` } ]

`how-proved` ::= "how_proved" : `json-string`

#### 3.2.2 The `file` object

Contains the name of the file (again, without path), where the checked rule
applies to.

* Example:
```json
"file": "saatana-crypto.adb"
```

#### 3.2.3 The `line` object

Contains the line number, where the checked rule applies to.

* Example:
```json
"line": 22
```

#### 3.2.4 The `col` object

Contains the column number, where the checked rule applies to.

* Example:
```json
"col": 9
```

#### 3.2.5 The `rule` object

Contains the identifier of the rule being checked.

* Example:
```json
"rule": "DEPENDS_WRONG"
```

Please note that you can get a list of these rules by calling gnatprove with
the switch `--list-categories`, so I am not going to list them here.

#### 3.2.6 The `severity` object

Contains the severeness of the proof result. As far as I have figured out,
`info` means no error, while `warning` and `error` have their usual meaning.
Other values than these three can possibly occur.

* Example:
```json
"severity": "info"
```

#### 3.2.7 The `entity` object

Contains the objects for `name` of the source file and the `location` within
that source file of the (enclosing) compilation unit.

* Example:
```json
"entity": {
  "name": "Saatana.Crypto.Oadd.Add_Carry",
  "sloc": # [...]
```
*Note*: There is some name mangling going on here, `Oadd` (*O*perator*add*) is
actually an user-defined operator `"+"`.

#### 3.2.7.1 The `name` object

Contains the name of the entity to which the VC applies.

* Example:
```json
"name": "Saatana.Crypto.Oadd.Add_Carry"
```

#### 3.2.7.2 The `sloc` array

Each element contains the objects `file` and `line` containing the location of
the definition of the entity.

* Example:
```json
"sloc": [
          {
            "file": "saatana-crypto.adb",
            "line": 18
          }
        ]
```

#### 3.2.8 The `check-tree` array

I am assuming this will always be empty for a flow analysis and only be
interesting in the [`proof`](spark_file_format.md#332-the-proofcheck-tree-array)
part.

#### 3.2.9 The `how-proved` object

Contains the way the VC was dicharged. Unsure which values this object can
have. So far I encountered only `flow` (which makes sense in a flow analysis
step).

* Example:
```json
"how_proved": "flow"
```

### 3.3 The `proof` array

Similar to the [`flow`](spark_file_format.md#32-the-flow-array) array this
contains results from the proof, for each VC checked.

The following objects have been seen in the wild:

* `file`
* `line`
* `col`
* `rule`
* `severity`
* `entity`
* [`check_tree`](spark_file_format.md#332-the-check-tree-array)
* [`check_file`](spark_file_format.md#333-the-check-file-check-line-check-col-objects)
* [`check_line`](spark_file_format.md#333-the-check-file-check-line-check-col-objects)
* [`check_col`](spark_file_format.md#333-the-check-file-check-line-check-col-objects)
* [`how_proved`](spark_file_format.md#334-the-how-proved-object)
* [`stats`](spark_file_format.md#335-the-stats-object)

#### 3.3.1 Grammar Summary

`proof` ::= "proof" : [ { `file`, `line`, `col`, `rule`, `severity`, `entity`,
                          `check-tree`,  `check-file`, `check-line`,
                          `check-col`,  `how-proved`, `stats` } ]

For the `file`, `line`, `col`, `rule`, `severity`, and `entity` objects see the
[`flow`](spark_file_format.md#32-the-flow-array) array section above, they
share the same grammar, it seems. `check-file`, `check-line`, and `check-col`
are structurally identical to `file`, `line`, and `col`.

(Actually, I think `flow` and `proof` are virtually identical, they just use
different parts of the same structure.)

`check-tree` ::= "check_tree" : [ { `proof-attempts`, `transformations` } ]

`proof-attempts` ::= { `prover` }

`transformations` ::= { `???` }

`prover` ::= `json-string` : { `result`, `steps`, `time` }

`result` ::= "result" : `json-string`

`steps` ::= "steps" : `json-int`

`time` ::= "time" : `json-float`

`stats` ::= "stats" : { `prover-name` { `count`, `max-steps`, `time` } }

`prover-name` ::= `json-string`

`count` ::= "count" : `json-int`

`max-steps` ::= "max_steps" : `json-int`

`time` ::= "time" : `json-float`

#### 3.3.2 The `check-tree` array

This array contains a list of unnamed objects which are further subdivided into
a [`proof_attempts`](spark_file_format.md#3321-the-proof-attempts-object) and a
[`transformations`](spark_file_format.md#3322-the-transformations-object)
object.

##### 3.3.2.1 The `proof-attempts` object

`proof-attempts` contains objects which are denoted with the prover name.
Weirdly, this is not expressed as a JSON array, but as an object containing an
unspecified number of other JSON objects. I am assuming that each of these
unnamed objects containing a `proof-attempts` and `transformations` object
corresponds to a code path proven individually.

* Example:
```json
"proof_attempts": {
  "CVC4": { # ...
          }
}
```

##### 3.3.2.1.1 The `proof-attempts.*` objects

Each object contained in the `proof-attempts` object contains the result of
running a specific prover and the object is named after the prover.

* Example:
```json
"CVC4": {
  "result": "Unknown (unknown)",
  "steps": 4602537,
  "time": 1.25920000000000E+02
}
```

As far as I have seen, `result` will be "Valid" if the proof was successful.
`steps` holds the number of proof steps the prover has done (what these steps
mean depends on the prover used), and `time` is obviously the (wall clock) time
the prover spent doing all this.

##### 3.3.2.2 The `transformations` object

**GNAT CE 2020**: May contain a `trivial_true` which seems empty. In that
case, the corresponding `proof_attempts` object should be empty, as no prover was
involved in proving this particular path. Such proof attempts are assigned to a
prover object "Trivial".

#### 3.3.3 The `check-file`, `check-line`, `check-col` objects

In the cases I have seen, these simply duplicate the previous entries `file`,
`line`, and `column`, but there's more to them. From
[`spark2014.py`](https://github.com/AdaCore/gps/blob/master/share/plug-ins/spark2014.py):

> We associate the real check location to the text of the message. The locations
  of the message is not always the same as the one of the check.

* Example:
```json
  "check_file": "saatana-crypto-phelix.adb",
  "check_line": 120,
  "check_col": 102,
```

#### 3.3.4 The `how-proved` object

This seems to contain the string "prover" in all cases. I am assuming that
these may contain different values if the VC was either proven manually, or
justified.

#### 3.3.5 The `stats` object

Contains, for each prover involved in a successful proof, an object with the
name of the prover, and the three fields `count` (`json-int`), `max-steps`
(`json-int`), and `max_time` (`json-float`).

* Example:
```json
"stats": {
  "CVC4": {
    "count": 4,
    "max_steps": 1,
    "max_time": 5.99999986588955E-02
  }
}
```

* `count` corresponds to the number of paths from the `check-tree` array where
  the prover could successfully proof the VC.

* `max_steps` is a version of the steps reported in the `proof_attempt` object
  transformed by a set of prover specific values.

* `max-time` contains the time of the path where the the prover spent its most
  time successfully proving it.  There are minor (i.e. negligible) variations
  compared to the value from the corresponding `proof_attempt`.

### 3.4 The `assumptions` array

Contains information about the assumptions made in the proof. In other words,
these were not proved themselves, these are conditions that must hold true if
the proof is to be trusted. This object only contains data if gnatprove was
called with the `--assumptions` option.

#### 3.4.1 Grammar Summary

`all-assumptions` ::= "assumptions" : { [ `assumptions` ], `claim` }

`assumptions` ::= "assumptions" : [ `predicate-info` ]

`claim` ::= "claim" : { `predicate-info` }

`predicate-info` ::= { `predicate`, `arg` }

`predicate` ::= "predicate" : `json-string`

`arg` ::= "arg" : { `name`, `sloc` }

`sloc` ::= "sloc" : [ { `file`, `line` } ]

`file` ::= "file" : `json-string`

`line` ::= "line" : `json-int`

#### 3.4.2 The `all-assumptions` array

Contains a list of `assumptions` and a `claims` objects. I am assuming that
this lists all assumptions that must hold true for the claim to be true.

##### 3.4.2.1 The `assumptions` array

Contains `predicate-info` objects.

##### 3.4.2.1.1 The `claim` object

Holds a `predicate-info` object.

##### 3.4.2.2 The `predicate-info` object

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

##### 3.4.2.2.1 The `predicate` object

Holds the identifier of a predicate. So far I've encountered:

* CLAIM_POST (post condition which must hold true)
* CLAIM_EFFECTS
* CLAIM_AORTE (Absence Of Run Time Errors)

##### 3.4.2.2.2 The `arg` object

Contain a ```name``` object denoting the name of the subunit concerned, and a
```sloc``` object which references the file and line of its declaration.

### 3.5 The `timings` object

Seems to contain global timings from the whole proof (i.e. summed up execution
times etc.) for the different stages in the proof.

#### 3.5.1 Grammar Summary

`timings` ::= { `marking`, `globals-basic`, `globals-advanced`,
                `flow-analysis`, `codepeer-results`,
                `init-why-sections`, `translation-of-standard`,
                `translation-of-compilation-unit`, `proof`,
                `gnatwhy3.transformations.*` }

`marking` ::= "marking" : `json-float`

`globals-basic` ::= "globals (basic)" : `json-float`

`globals-advanced` ::= "globals/properties (advanced)" : `json-float`

`flow-analysis` ::= "flow analysis" : `json-float`

`codepeer-results` ::= "codepeer results" : `json-float`

`init-why-sections` ::= "init_why_sections" : `json-float`

`translation-of-standard` ::= "translation of standard" : `json-float`

**GNAT CE 2019**: `translation-of-compilation-unit` ::= "translation of compilation unit" : `json-float`

**GNAT CE 2019**: `proof` ::= "proof" : `json-float`

**GNAT CE 2020**: `gnatwhy3.transformations.*` ::= "gnatwhy3.transformations.*" : `json-float`

##### 3.5.2 The timings.* objects

All these objects contain a `json-float` designating the number of seconds spent
in this stage.

| Object name                       | Description |
| --- | --- |
| `marking`                         | *???* |
| `globals-basic`                   | The total time spent in the *basic* analysis of global effects. |
| `globals-advanced`                | The total time spent in the *advanced* analysis of global effects. |
| `flow-analysis`                   | Total time spent in flow analysis. |
| `codepeer-results`                | Total time spent in [CodePeer](https://www.adacore.com/codepeer/). |
| `init-why-sections`               | *???* |
| `translation-of-standard`         | The total time spent in translating the `Standard` package (and possibly other standard Ada library packages) into the intermediate language representation. |
| `translation-of-compilation-unit` | The total time spent in translating the analyzed unit into the intermediate language representation. |
| `proof`                           | The total time spent in proof (i.e. absence of run-time errors and functional analysis). |
| `gnatwhy3.transformations.*` | Time spent in several parts of why3.  At the moment, the the exact meaning of all the fields is unclear. |
