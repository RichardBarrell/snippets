#!/usr/bin/env python
import re, os, os.path, sys

# Open a list of files in Emacs buffers, flipping between open windows for
# each successive file, from the command-line through emacsclient.

esc = re.compile(r'["\\]')

if __name__ == '__main__':
    fs = []
    for arg in sys.argv[1:]:
        arg = os.path.abspath(arg)
        fs.append('(other-window 1)(find-file "%s")' % esc.sub(lambda m: '\\'+m.group(0), arg))
    ff = '(progn %s)' % "".join(fs)
    os.execvp('emacsclient', ('emacsclient', '-n', '-e', ff))
