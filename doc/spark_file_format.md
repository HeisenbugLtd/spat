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

### The `spark` object

Contains an array of JSON objects where each(?) object contains
information about the Ada unit being analyzed: The `name` of the unit,
the source location `sloc` and **presumably** the options used for the
SPARK analysis in another object called `spark`.

* I do not expect the array embedded in the object to ever contain more
  than one entry.  I suspect, storing it as an array is purely
  incidental.  At least from my point of view it does not make sense to
  have more than one unit mentioned here.

#### The `spark[].name` object

Contains the Ada name of the unit being analyzed.

#### The `spark[].sloc[]` object

`spark[].sloc` is an array containing JSON objects with source location
info. Each element contains a `file` and a `line` object, containing the
source file name (without any path) and the line within that file.

As far as I have figured out, this simply points to the line where the
currently analyzed Ada unit is defined, e.g. the line of an Ada `package`
specification.

* As above, I do not expect the array embedded in the object to ever
  contain more than one entry, otherwise it would be indicating to more
  than one declaring entity.

#### Grammar

`spark` ::= `ada_unit`**[]** # read: array of ada_unit
`ada_unit` ::= { `name`, `sloc`, `spark` }
`name` ::= <"name" : `json-string`>
`sloc` ::= `source-location`**[]** # read: array of source-location
`source-location` ::= { `file`, `line` }
`file` ::= <"file" : `json-string`>
`line` ::= <"line" : `json-int`>

### The `flow` object

### The `proof` object

### The `assumptions` object

### The `timings` object
