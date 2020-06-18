#!/usr/bin/env bash

#
# Regression tests
#
# Compare the results of different projects and spat runs with stored templates.
#

# Remove possibly leftover temp files
rm -f test.diff *.out

# First argument is the project directory
# Second argument the project file (.gpr) within that project.
run_check () {
  # output w/o sorting options is not deterministic (hash function, which may differ between compilers)
  for SUMMARY in "" "-s"; do # w/o summary, with summary
    for REPORT in "" "-ra" "-rf" "-ru" "-rj"; do # report none, all, failed, unproved, unjustified
      if [[ -n "${SUMMARY}" || -n ${REPORT} ]]; then # neither report nor summary, skip that
        for DETAILS in "" "-d"; do # details off, details on
          for SORTING in "-ca" "-cs" "-ct"; do # sort alphabetical, by success time, by max time
            SPAT_OPTIONS=`echo "${SUMMARY} ${REPORT} ${DETAILS} ${SORTING}" | sed -e "s/ \+/ /g;s/^ *//;s/ *$//"`
            OPT_NAME="$1".`echo "${SPAT_OPTIONS}" | sed -e "s/[- ]//g"`

            # (older reference version for template generation)
            # run_spat ${SPAT_OPTIONS} -P "$1/$2" > "spat.${OPT_NAME}.template"

            # Run test
            echo "Testing \"$1\" with options \"${SPAT_OPTIONS}\"..." # Show some progress.
            ../obj/run_spat ${SPAT_OPTIONS} -P "$1/$2" > "spat.${OPT_NAME}.out"

            # Show template differences (FIXME: 'diff' might not be installed)
            (git diff --no-index "spat.${OPT_NAME}.template" "spat.${OPT_NAME}.out") >> test.diff || RESULT=$?
          done # sorting
        done # details
      fi
    done # report
  done # summary
}

RESULT=0

# Recompile to be sure we test the latest version.
gprbuild -P ../spat.gpr

run_check "test-saatana" "saatana.gpr"
run_check "test-sparknacl" "src/sparknacl.gpr"

case ${RESULT} in
[0-1])
  if [ -s test.diff ]; then
    echo "Test failed, there are differences."
    cat test.diff
    RESULT=1
  else
    echo "Test succeeded."
    RESULT=0
  fi
  ;;
*)
  echo "One or more diffs failed."
esac

# Remove temp files
rm -f test.diff spat*.out

exit ${RESULT}
