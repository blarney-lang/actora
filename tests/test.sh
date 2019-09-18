#!/bin/bash

ELITE=../compiler/elite

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

make -s -C ../compiler > /dev/null
if [ ! -f "$ELITE" ]; then
  echo Cannot find compiler: $ELITE
fi

# Regenerate expected outputs
if [ "$1" == "regen" ]; then
  for PROG in $(ls *.erl); do
    BASE=$(basename $PROG .erl)
    $ELITE -r $PROG > out/$BASE.out
  done
  exit -1
fi

# Check correctness of compiler/semantics
echo "TESTING COMPILER AND SMALL-STEP SEMANTICS"
rm -f *.got
for PROG in $(ls *.erl); do
  BASE=$(basename $PROG .erl)
  echo -n "$BASE: "
  $ELITE -r $PROG > $BASE.got
  if cmp $BASE.got out/$BASE.out; then
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
  if cmp $BASE.got out/$BASE.out; then
    echo -e "${GREEN}OK${NC}"
  else
    echo -e "${RED}FAILED${NC}"
    exit -1
  fi
done
rm -f *.got
rm -rf tmp

# Check correctness of emulator
echo ""
echo "TESTING STACK MACHINE EMULATOR"
make -s -C ../emulator/
rm -f *.got
rm -rf act
mkdir act
for PROG in $(ls *.erl); do
  BASE=$(basename $PROG .erl)
  echo -n "$BASE: "
  $ELITE -b $PROG > act/$BASE.act
  ../emulator/actemu act/$BASE.act | head -n 1 > $BASE.got
  if cmp $BASE.got out/$BASE.out; then
    echo -e "${GREEN}OK${NC}"
  else
    echo -e "${RED}FAILED${NC}"
    exit -1
  fi
done
rm -f *.got
rm -rf act

# Checking correctness of core
echo ""
echo "TESTING CORE"
rm -f *.got
rm -rf CoreSim
make -s -C ../rtl > /dev/null
cd ../rtl && ./Main && cd ../tests
cp -r ../rtl/CoreSim-Verilog .
make -s -C CoreSim-Verilog > /dev/null 2> /dev/null
for PROG in $(ls *.erl); do
  BASE=$(basename $PROG .erl)
  echo -n "$BASE: "
  $ELITE -h $PROG > CoreSim-Verilog/instrs.hex
  cd CoreSim-Verilog
  ./CoreSim | grep Result | cut -d ' ' -f 3 > ../$BASE.got
  cd ..
  if cmp $BASE.got out/$BASE.out; then
    echo -e "${GREEN}OK${NC}"
  else
    echo -e "${RED}FAILED${NC}"
    exit -1
  fi
done
rm -r *.got
rm -rf CoreSim-Verilog
