#!/bin/bash

ELITE=../compiler/elite

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

if [ ! -f "$ELITE" ]; then
  echo Cannot find compiler: $ELITE
fi

# Regenerate expected outputs
if [ "$1" == "regen" ]; then
  for PROG in $(ls *.erl); do
    BASE=$(basename $PROG .erl)
    $ELITE $PROG > $BASE.out
  done
  exit -1
fi

# Check correctness of compiler/semantics
echo "TESTING COMPILER AND SMALL-STEP SEMANITCS"
rm -f *.got
for PROG in $(ls *.erl); do
  BASE=$(basename $PROG .erl)
  echo -n "$BASE: "
  $ELITE -r $PROG > $BASE.got
  if cmp $BASE.got $BASE.out; then
    echo -e "${GREEN}OK${NC}"
  else
    echo -e "${RED}FAILED${NC}"
    exit -1
  fi
done
rm -f *.got

# Check correctness of C backend
echo ""
echo "TESTING C BACKEND"
rm -f *.got
rm -rf tmp
mkdir tmp
for PROG in $(ls *.erl); do
  BASE=$(basename $PROG .erl)
  echo -n "$BASE: "
  $ELITE -c tmp/$BASE $PROG
  make -s -C tmp/$BASE
  tmp/$BASE/main > $BASE.got
  if cmp $BASE.got $BASE.out; then
    echo -e "${GREEN}OK${NC}"
  else
    echo -e "${RED}FAILED${NC}"
    exit -1
  fi
done
rm -f *.got
rm -rf tmp
