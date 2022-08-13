#!/usr/bin/python3

import sys

print("""Usage: scripts/increment-config-nums.py src/main/scala/test/Configs.scala " TestConfig" 66""")

cfg = sys.argv[2]
incr_from = int(sys.argv[3])

f = open(sys.argv[1], "r")
lines = f.readlines()
f.close()
out = []
for line in lines:
    cfg_str = "{}{}".format(cfg, str(incr_from).zfill(2))
    next_cfg_str = "{}{}".format(cfg, str(incr_from + 1).zfill(2))
    if cfg_str in line:
        line = line.replace(cfg_str, next_cfg_str)
        incr_from += 1
    out.append(line.rstrip())

f = open(sys.argv[1], "w")
f.write('\n'.join(out))
