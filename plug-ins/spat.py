#!/usr/bin/python

"""Provides a GNATStudio plugin for SPAT (SPARK Proof Analysis Tool).
"""

##############################################################################
# No user customization below this point
##############################################################################

import GPS
from gs_utils import interactive
import re

list_unproved = 'List Unproved'
list_all      = 'List All'

# Name of the messages console
MESSAGES = "Messages"
fname_pattern=r"([a-z0-9-]+\.ad[bs]):(\d+):(\d+)"
line_pattern=r"^.*" + fname_pattern + r".*$" # For GPS.Process pattern match.
VC_pattern=r"^.*(VC_[A-Z_]+)\ +" + fname_pattern + r" +=> +(.*)$"
# proof checks all start with 'VC_'

vc_time_pattern = re.compile(VC_pattern)
vc_dictionary = dict()

def on_match(self, matched, unmatched):
   match = vc_time_pattern.search(matched)

   if match:
      vc, file, line, column, times = match.group (1, 2, 3, 4, 5)

      if not vc_dictionary.has_key (vc):
         #  Set dictionary entry to same string if not known.
         vc_dictionary[vc] = vc

      GPS.Locations.add (category="SPAT Results",
                         file=GPS.File (file),
                         line=int(line),
                         column=int(column),
                         message=vc_dictionary[vc] + " => " + times)

def run_spat(options):
   proc = GPS.Process (command="run_spat " + options,
                       regexp=line_pattern,
                       on_match=on_match,
                       show_command=True)

@interactive("Show All", menu="/SPARK/SPAT/Show All")
def run_spat_all():
   options="-ra -ct -d -P " + GPS.Project.root().file().path
   run_spat(options)

@interactive ("Show Unproved", menu="/SPARK/SPAT/Show Unproved")
def run_spat_unproved():
   options="-ru -ct -d -P " + GPS.Project.root().file().path
   run_spat(options)

def on_project_changed (hook_name):
   # Call gnatprove with "--list-categories to translated VC names."
   try:
      proc = GPS.Process (command="gnatprove --list-categories",
                          show_command=True)
      result = proc.get_result()

      for line in result.split ("\n"):
         description = line.split ("-")

         if len(description) == 3:
            vc_dictionary[description[0].strip()] = description[1].strip()
   except:
      GPS.Console(MESSAGES).write ("spat.py: Could not retrieve list of categories.",
                                   "error")

GPS.Hook ("project_changed").add (on_project_changed)
