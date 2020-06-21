#!/usr/bin/python

"""Provides a GNATStudio plugin for SPAT (SPARK Proof Analysis Tool).
"""

##############################################################################
# No user customization below this point
##############################################################################

import GPS
from gs_utils import interactive
import math, re

list_unproved = 'List Unproved'
list_all      = 'List All'

# Name of the messages console
MESSAGES = "Messages"
fname_pattern=r"([a-z0-9-]+\.ad[bs]):(\d+):(\d+)"
line_pattern=r"^.*" + fname_pattern + r".*$" # For GPS.Process pattern match.
VC_pattern=r"^.*(VC_[A-Z_]+)\ +" + fname_pattern + r" +=> +(.*)$"
# proof checks all start with 'VC_'

class SPAT_Location:
   def __init__(self, file, line, column, vc, times):
      max_success, max_proof, total = times.split ("/")
      self.file = file
      self.line = int(line)
      self.column = int(column)
      self.vc = vc
      self.times = float(max_proof)

def by_time(elem):
   return elem.times

class SPAT_Parser:
   def __init__(self):
      self.vc_time_pattern = re.compile(VC_pattern)
      self.vc_dictionary = dict()
      self.loc_list = list()

      try:
         proc = GPS.Process (command="gnatprove --list-categories")
         result = proc.get_result()

         for line in result.split ("\n"):
            description = line.split ("-")

            if len(description) == 3:
               self.vc_dictionary[description[0].strip()] = description[1].strip()
      except:
         GPS.Console(MESSAGES).write ("spat.py: Could not retrieve list of categories.",
                                      "error")

   def on_match(self, process, matched, unmatched):
      match = self.vc_time_pattern.search(matched)
      # Parse found locations and times into a SPAT_Location object
      if match is not None:
         vc, file, line, column, times = match.group (1, 2, 3, 4, 5)

         # Add location object to the list
         loc = SPAT_Location(file, line, column, vc, times)
         self.loc_list.append(loc)
      else:
         # GPS.Console(MESSAGES).write ("on_match called, but no match found\n")
         pass

   def run(self, options):
      # Clear previous results (if any)
      categories = GPS.Locations.list_categories()
      
      for category in categories:
         if category.startswith("SPAT Results ["):
            GPS.Locations.remove_category (category)

      self.loc_list = list() # Clear list.
      proc = GPS.Process (command="run_spat -R " + options,
                          regexp=line_pattern,
                          on_match=self.on_match,
                          show_command=True)
      result = proc.wait()

      if result == 0 and len(self.loc_list) > 0:
         self.loc_list.sort(key=by_time, reverse=True)

         # List is sorted, so maximum and minimum are at their ends
         histogram = list()
         hist_last = int(math.ceil (self.loc_list[0].times))

         hist_high = 1
         hist_low = hist_high / 2

         while hist_low < hist_last:
            histogram = ([x for x in self.loc_list if hist_low <= x.times < hist_high])
            category = "SPAT Results [" + str(hist_low) + " s .. " + str(hist_high) + " s]"

            for loc in histogram:
               if not self.vc_dictionary.has_key (loc.vc):
                  #  Set dictionary entry to same string if not known.
                  self.vc_dictionary[vc] = loc.vc

               GPS.Locations.add(category=category,
                                 file=GPS.File (loc.file),
                                 line=loc.line,
                                 column=loc.column,
                                 message=self.vc_dictionary[loc.vc] + " took " + str(loc.times) + " s")

            hist_low  = hist_high
            hist_high = hist_high * 2

# Create singleton instance of parser
spat_parser = SPAT_Parser()

@interactive("Show All", menu="/SPARK/SPAT/Show All")
def run_spat_all():
   options="-ra -ct -d -P " + GPS.Project.root().file().path
   spat_parser.run(options)

@interactive ("Show Unproved", menu="/SPARK/SPAT/Show Unproved")
def run_spat_unproved():
   options="-ru -ct -d -P " + GPS.Project.root().file().path
   spat_parser.run(options)
