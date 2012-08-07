#!/usr/bin/env python
import re, os, os.path, sys, tempfile
from subprocess import Popen, PIPE

# This is ugly as sin but it's what I've been using for ages to grep code and
# put the result into a convenient Emacs buffer. Don't look too hard.

esc = re.compile(r'["\\]')
q = lambda s: esc.sub(lambda m: '\\'+m.group(0), s)

if __name__ == '__main__':
    fd,fname = tempfile.mkstemp()
    pattern = sys.argv[1]
    # find -L . -type f -print0 | egrep -zv "(/\\.svn/|\\.py[co]$|\\.pot?$)" | xargs -0 egrep -nH
    f = Popen(["/usr/bin/find","-L",".","-type","f","-print0"],stdout=PIPE)
    e = Popen(["/bin/egrep","-zv","(/\\.svn/|\\.py[co]$|\\.pot?$)"],
              stdin=f.stdout,
              stdout=PIPE)
    x = Popen(["/usr/bin/xargs","-0","/bin/egrep","-nHe",pattern],
              stdin=e.stdout,
              stdout=fd)
    f.wait()
    e.wait()
    x.wait()
    os.close(fd)
    ff = """(progn
(switch-to-buffer (generate-new-buffer "*grep-nom*"))
(insert-file-contents "%s")(grep-mode))""" % (q(fname),)
    Popen(['emacsclient', '-n', '-e', ff]).wait()
    os.unlink(fname)
