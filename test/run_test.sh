#!/usr/bin/env bash

#
# Regression tests
#
# Compare the results of different projects and spat runs with stored templates.
#

# Remove possibly leftover temp files
rm -f *.diff *.out

# Arguments: project directory ($1), project file ($2), options ($3)
single_check () {
  RESULT=0
  echo "Testing \"$1\" with options \"$3\"..." # Show some progress.
  OPT_NAME="$1".`echo "$3" | sed -e "s/[- \.\*]//g"`

  # ../obj/run_spat $3 -P "$1/$2" > "spat.${OPT_NAME}.template" # Recreate templates
  ../obj/run_spat $3 -P "$1/$2" > "spat.${OPT_NAME}.out"

  # Show template differences
  (git diff --no-index "spat.${OPT_NAME}.template" "spat.${OPT_NAME}.out") > spat.${OPT_NAME}.diff || RESULT=$?

  if [ ${RESULT} -ne 0 ]; then
    cat spat.${OPT_NAME}.diff # Leave files on disk for further inspection.
  else
    # Remove temp files
    rm -f spat.${OPT_NAME}.out
    rm -f spat.${OPT_NAME}.diff
  fi

  return ${RESULT}
}

# First argument is the project directory
# Second argument the project file (.gpr) within that project.
run_check () {
  PIDS=""

  # output w/o sorting options is not deterministic (hash function, which may differ between compilers)
  for SUMMARY in "" "-s"; do # w/o summary, with summary
    for REPORT in "" "-ra" "-rf" "-ru" "-rj"; do # report none, all, failed, unproved, unjustified
      if [[ -n "${SUMMARY}" || -n ${REPORT} ]]; then # neither report nor summary, skip that
        for DETAILS in "" "-d" "-d 1"; do # details off, full details, details level 1
          for SORTING in "-ca" "-cs" "-ct" "-cp" "-cq"; do # sort alpha, success time, max time, success steps, max steps
              SPAT_OPTIONS=`echo "${SUMMARY} ${REPORT} ${DETAILS} ${SORTING}" | sed -e "s/ \+/ /g;s/^ *//;s/ *$//"`

              single_check "$1" "$2" "${SPAT_OPTIONS}" &
              PIDS="${PIDS} $!"
          done # sorting
        done # details
      fi
    done # report
  done # summary

  for JOB in ${PIDS}; do
    wait ${JOB} || return 1
  done
}

# First argument is the project directory
# Second argument the project file (.gpr) within that project.
run_cut_off_check () {
  PIDS=""

  for REPORT in "-ra" "-rf" "-ru" "-rj"; do # report all, failed, unproved, unjustified
    for SORTING in "-ca" "-cs" "-ct" "-cp" "-cq"; do # sort alpha, success time, max time, success steps, max steps
      for CUT_OFF in "-p 500ms" "-p 1" "-p 5s"; do # some cut-off options
        for DETAILS in "-d 1" "-d"; do # detail level 1, full details
          SPAT_OPTIONS=`echo "${REPORT} ${DETAILS} ${SORTING} ${CUT_OFF}"`

          single_check "$1" "$2" "${SPAT_OPTIONS}" &
          PIDS="${PIDS} $!"
        done # details
      done # cut-off
    done # sorting
  done # report

  for JOB in ${PIDS}; do
    wait ${JOB} || return 1
  done
}

# Recompile to be sure we test the latest version.
gprbuild -P ../spat.gpr

# Basic checks first.
run_check "test-saatana" "saatana.gpr"         || exit 1
run_check "test-sparknacl" "src/sparknacl.gpr" || exit 2

# From issue reports
run_check "test-issues" "issues.gpr" || exit 3

# --cut-off checks
run_cut_off_check "test-saatana" "saatana.gpr" || exit 4
run_cut_off_check "test-sparknacl" "src/sparknacl.gpr" || exit 5

# --suggest checks
single_check "test-saatana" "saatana.gpr" "-g" || exit 6
single_check "test-sparknacl" "src/sparknacl.gpr" "-g" || exit 7
single_check "test-issues" "issues.gpr" "-g" || exit 8

# Very simple --entity checks
single_check "test-saatana" "saatana.gpr" "-ra -e .*Setup.*" || exit 9
single_check "test-sparknacl" "src/sparknacl.gpr" "-ra -e .*ASR.*" || exit 10
single_check "test-issues" "issues.gpr" "-ra -e .*U64.*" || exit 11
