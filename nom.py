#!/usr/bin/env python
import re, os, os.path, sys, tempfile
import subprocess

# This script dumps its stdin into an Emacs buffer (using emacsclient).
# cat foo | nom # creates a buffer called *nom*
# cat foo | nom python # creates a buffer called *nom* in python-mode
# cat foo | nom python yo # creates a buffer called *nom-yo* in python-mode

esc = re.compile(r'["\\]')
q = lambda s: esc.sub(lambda m: '\\'+m.group(0), s)

if __name__ == '__main__':
    fd, fname = tempfile.mkstemp()
    subprocess.Popen(["dd"], stdout=fd).wait()
    os.close(fd)

    name = ""
    mode = ""
    if len(sys.argv) > 1:
        mode = "(" + sys.argv[1] + "-mode)"
    if len(sys.argv) > 2:
        name = "-" + sys.argv[2]

    ff = """(progn
(switch-to-buffer (generate-new-buffer "*nom%s*"))
(insert-file-contents "%s")%s)""" % (q(name), q(fname), mode)
    subprocess.Popen(['emacsclient', '-n', '-e', ff]).wait()
    os.unlink(fname)
