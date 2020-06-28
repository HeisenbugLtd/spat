#!/usr/bin/env bash

#
# Regression tests
#
# Compare the results of different projects and spat runs with stored templates.
#

# Remove possibly leftover temp files
rm -f test.diff *.out

# Arguments: project directory ($1), project file ($2), options ($3)
single_check () {
  echo "Testing \"$1\" with options \"$3\"..." # Show some progress.
  OPT_NAME="$1".`echo "$3" | sed -e "s/[- \.\*]//g"`

  # run_spat $3 -P "$1/$2" > "spat.${OPT_NAME}.template" # Recreate templates
  ../obj/run_spat $3 -P "$1/$2" > "spat.${OPT_NAME}.out"

  # Show template differences
  (git diff --no-index "spat.${OPT_NAME}.template" "spat.${OPT_NAME}.out") > test.diff || RESULT=$?

  # Remove temp files
  rm -f spat*.out

  # Abort if not successful so far.
  if [ ${RESULT} -ne 0 ]; then
    cat test.diff # Leave file on disk for further inspection.
    exit ${RESULT}
  fi
}

# First argument is the project directory
# Second argument the project file (.gpr) within that project.
run_check () {
  # output w/o sorting options is not deterministic (hash function, which may differ between compilers)
  for SUMMARY in "" "-s"; do # w/o summary, with summary
    for REPORT in "" "-ra" "-rf" "-ru" "-rj"; do # report none, all, failed, unproved, unjustified
      if [[ -n "${SUMMARY}" || -n ${REPORT} ]]; then # neither report nor summary, skip that
        for DETAILS in "" "-d" "-d 1"; do # details off, full details, details level 1
          for SORTING in "-ca" "-cs" "-ct"; do # sort alphabetical, by success time, by max time
              SPAT_OPTIONS=`echo "${SUMMARY} ${REPORT} ${DETAILS} ${SORTING}" | sed -e "s/ \+/ /g;s/^ *//;s/ *$//"`

              single_check "$1" "$2" "${SPAT_OPTIONS}"
          done # sorting
        done # details
      fi
    done # report
  done # summary
}

# First argument is the project directory
# Second argument the project file (.gpr) within that project.
run_cut_off_check () {
  for REPORT in "-ra" "-rf" "-ru" "-rj"; do # report all, failed, unproved, unjustified
    for SORTING in "-ca" "-cs" "-ct"; do # sort alphabetical, by success time, by max time
      for CUT_OFF in "-p 500ms" "-p 1" "-p 5s"; do # some cut-off options
        SPAT_OPTIONS=`echo "${REPORT} -d ${SORTING} ${CUT_OFF}"`

        single_check "$1" "$2" "${SPAT_OPTIONS}"
      done # cut-off
    done # sorting
  done # report
}

RESULT=0

# Recompile to be sure we test the latest version.
gprbuild -P ../spat.gpr

# Basic checks first.
run_check "test-saatana" "saatana.gpr"
run_check "test-sparknacl" "src/sparknacl.gpr"

# From issue reports
run_check "test-issues" "issues.gpr"

# --cut-off checks
run_cut_off_check "test-saatana" "saatana.gpr"
run_cut_off_check "test-sparknacl" "src/sparknacl.gpr"

# --suggest checks
single_check "test-saatana" "saatana.gpr" "-g"
single_check "test-sparknacl" "src/sparknacl.gpr" "-g"
single_check "test-issues" "issues.gpr" "-g"

# Very simple --entity checks
single_check "test-saatana" "saatana.gpr" "-ra -e .*Setup.*"
single_check "test-sparknacl" "src/sparknacl.gpr" "-ra -e .*ASR.*"
single_check "test-issues" "issues.gpr" "-ra -e .*U64.*"

exit ${RESULT}
