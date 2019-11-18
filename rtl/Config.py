#!/usr/bin/env python2

import sys

# Circuit-generator parameters
p = {}

# Command-line options
if len(sys.argv) > 2:
  if sys.argv[1] == "sim":
    p["SimMode"] = 1
  mode = sys.argv[2]
else:
  print "Usage: config.py <sim|syn> <hw-cpp|cpp>"
  sys.exit(-1)

# Instruction memory size (in instructions)
p["LogInstrMemSize"] = 11

# Stack size (in cells)
p["LogStackSize"] = 11

# Heap size (in cells)
p["LogHeapSize"] = 13

# Heap size (in cell pairs)
p["LogHeapSizeMinusOne"] = p["LogHeapSize"] - 1

# GC scratchpad size (in cell pairs)
p["LogScratchpadSizeMinusOne"] = p["LogHeapSizeMinusOne"] - 1

# GC threshold (GC invoked when heap size passes this)
p["GCThreshold"] = (
    (2 ** p["LogHeapSizeMinusOne"]) - (2 ** (p["LogStackSize"]-1)) - 32
  )

# DRAM parameters
p["DRAMReqIdBits"] = 4
p["DRAMBeatWidth"] = 9
p["DRAMBurstWidth"] = 4
p["DRAMSimLatency"] = 25
p["DRAMLogMaxInFlight"] = 5
if "SimMode" in p:
  p["LogBeatsPerDRAM"] = 13
else:
  p["LogBeatsPerDRAM"] = 23

# Emit parameters in desired format
if mode == "hw-cpp":
  for var in p:
    print("#define " + var + " " + str(p[var]))
elif mode == "cpp":
  for var in p:
    print("#define Act" + var + " " + str(p[var]))
