#!/usr/bin/env python2
import sys
import re
import os

if len(sys.argv) < 2 or not re.match(r"\d{4}-\d\d-\d\d", sys.argv[1]):
    print "Usage: git daylog 2013-01-01 ..."
    sys.exit(1)

day = sys.argv[1]
after = "--after=%s 00:00" % day
before = "--before=%s 23:59" % day
os.execlp("git", "git", "log", after, before, *sys.argv[2:])
