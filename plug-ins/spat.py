#!/usr/bin/python

"""Provides a GNATStudio plugin for SPAT (SPARK Proof Analysis Tool).
"""

# Copyright (C) 2020 by Heisenbug Ltd.
# Licensed under WTFPL

##############################################################################
# No user customization below this point
##############################################################################

import math
import re

import GPS
from gs_utils import interactive

# Name of the messages console
MESSAGES = "Messages"

# Regexp patterns.
FNAME_PATTERN = r"([a-z0-9-]+\.ad[bs]):(\d+):(\d+)"
LINE_PATTERN = r"^.*" + FNAME_PATTERN + r".*$" # For GPS.Process pattern match.
VC_PATTERN = r"^.*(VC_[A-Z_]+)\ +" + FNAME_PATTERN + r" +=> +(.*)$"
# proof checks all start with 'VC_'

class SPATLocation:
    """A file location with associated time(s)."""
    def __init__(self, file, line, column, vc, times):
        max_success, max_proof, total = times.split("/")
        self.file = file
        self.line = int(line)
        self.column = int(column)
        self.vc = vc
        self.times = float(max_proof)

def by_time(elem):
    """
    Returns associated times field of elem (elem assumed to be an instance of SPATLocation.
    """
    return elem.times

class SPATParser:
    """Parses output of SPAT into some file, line, column, times stuff."""
    def __init__(self):
        self.vc_time_pattern = re.compile(VC_PATTERN)
        self.vc_dictionary = dict()
        self.loc_list = list()

        try:
            proc = GPS.Process(command="gnatprove --list-categories")
            result = proc.get_result()

            for line in result.split("\n"):
                description = line.split("-")

                if len(description) == 3:
                    self.vc_dictionary[description[0].strip()] = description[1].strip()
        except:
            GPS.Console(MESSAGES).write("spat.py: Could not retrieve list of categories.",
                                        "error")

    def on_match(self, process, matched, unmatched):
        """
        Match callback called by GPS.Process when a matching string has been found in the output.
        """
        match = self.vc_time_pattern.search(matched)
        # Parse found locations and times into a SPAT_Location object
        if match is not None:
            vc, file, line, column, times = match.group(1, 2, 3, 4, 5)

            # Add location object to the list
            loc = SPATLocation(file, line, column, vc, times)
            self.loc_list.append(loc)
        else:
            # GPS.Console(MESSAGES).write ("on_match called, but no match found\n")
            pass

    def run(self, options):
        """Run SPAT with given extra options and parse the output."""
        # Clear previous results (if any)
        categories = GPS.Locations.list_categories()

        for category in categories:
            if category.startswith("SPAT Results ["):
                GPS.Locations.remove_category(category)

        self.loc_list = list() # Clear list.
        proc = GPS.Process(command="run_spat -R " + options,
                           regexp=LINE_PATTERN,
                           on_match=self.on_match,
                           show_command=True)
        result = proc.wait()

        if result == 0 and len(self.loc_list) > 0:
            self.loc_list.sort(key=by_time, reverse=True)

            # List is sorted in descending order, starts with highest value
            bucket_high = int(math.ceil(self.loc_list[0].times))

            # Slice indices
            start = index = 0

            # While there is still a bucket to fill.
            while bucket_high > 0:
                bucket_low = bucket_high / 2 # Half the range on each iteration

                # Find last index to value fitting into current bucket.
                while index < len(self.loc_list) and self.loc_list(index) > bucket_low:
                    index = index + 1

                # Range not empty?
                if index > start:
                    # Add all locations in the slice to their new category
                    category = "SPAT Results [" + str(bucket_low) + " s .. " + str(bucket_high) + " s]"

                    for loc in self.loc_list[start:index]:
                        if not self.vc_dictionary.has_key(loc.vc):
                            #  Set dictionary entry to same string if not known.
                            self.vc_dictionary[loc.vc] = loc.vc

                        GPS.Locations.add(category=category,
                                          file=GPS.File(loc.file),
                                          line=loc.line,
                                          column=loc.column,
                                          message=self.vc_dictionary[loc.vc] + " took " + str(loc.times) + " s")

                # Move start position and bucket interval
                start = index
                bucket_high = bucket_low

# Create singleton instance of parser
SPAT_PARSER = SPATParser()

@interactive("Show All", menu="/SPARK/SPAT/Show All")
def run_spat_all():
    """Run SPAT with report mode 'all' option."""
    options = "-ra -ct -d -P " + GPS.Project.root().file().path
    SPAT_PARSER.run(options)

@interactive("Show Unproved", menu="/SPARK/SPAT/Show Unproved")
def run_spat_unproved():
    """Run SPAT with report mode 'unproved' option."""
    options = "-ru -ct -d -P " + GPS.Project.root().file().path
    SPAT_PARSER.run(options)
