#!/bin/bash

# Prerequisites
# - fpc (Free Pascal compiler)
# - lazbuild (Lazarus)

# Compiler arguments used explanation:
# -Sd
#   use Delphi mode
# -B
#   build all modules
# -FE"./target"
#   use "target" as a output dir for units and exe file
# -O3
#   optimization group 3 (safe optimizations)
# -o
#   output executable file name
# -vm3175,3177 
#   do not display warnings 3175 ('Some fields coming before "xxx" were not initialized') and 3177 ('Some fields coming after "xxx" were not initialized') 

echo "Building console version..."
mkdir -p target
(cd source && fpc -Sd -B -FE"../target" -O3 -o"tdascon" -vm3175,3177 TatraDAS.dpr)

echo "Building GUI version..."
lazbuild source/TatraDAS.lpi

# Build unit tests project
echo "Building unit tests..."
mkdir -p target/test
(cd test && fpc -Sd -FE"../target/test" -O3 -vm3175,3177 TestTatraDAS.dpr)

# Run unit tests
echo "Running unit tests..."
mkdir -p target/test-report
(cd target/test && ./TestTatraDAS --all --format=plain > ../test-report/plain.txt && cat ../test-report/plain.txt)

# Check unit tests result
echo "Checking unit tests result..."
(cd target/test-report && cat plain.txt | grep "Number of errors:    0" && cat plain.txt | grep "Number of failures:  0")
UNIT_TESTS_RESULT=$?
if [ ${UNIT_TESTS_RESULT} -ne 0 ]; then 
    echo "!!! Unit tests failed:"
    cat target/test-report/plain.txt
    exit 1
fi
