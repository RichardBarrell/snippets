from subprocess import Popen, PIPE, check_call
import os
import sys
import time
import tempfile

# e.g. python ie6vnc.py "IE6 - WinXP" 1280x800 1
# then 'vncviewer :1'

vm, resolution, port = sys.argv[1:]

port = ":%d" % int(port)

n = tempfile.NamedTemporaryFile()
vpw = Popen(["vncpasswd", n.name], stdin=PIPE)
vpw.stdin.write("password1\n")
vpw.stdin.write("password1\n")
vpw.stdin.write("\n")
vpw.wait()

xvnc = Popen([
    "Xvnc", "-geometry", resolution,
    "-Passwordfile", n.name, port,
])
env = dict(os.environ)
env['DISPLAY'] = port

try:
    while True:
        check_call(["VBoxManage", "snapshot", vm, "restorecurrent"])
        vbox = Popen(
            [
                "VBoxSDL", "--fullscreen", "--fullscreenresize",
                "--startvm", vm,
            ],
            env=env,
        )
        vbox.wait()
        if vbox.returncode != 0:
            raise Exception("oh no, VBox failed")
except:
    try:
        vbox.kill()
        vbox.wait()
    except:
        pass

xvnc.kill()
xvnc.wait()

print "done"
