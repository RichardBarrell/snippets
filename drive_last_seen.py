import struct
import socket
import random


def query(qry):
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.connect(("localhost", 1070))
    s.send(qry)
    result = s.recv(16)
    s.close()
    return result

def hit():
    nick = str(random.randint(0,1<<20))
    if query('j' + nick) != ':)              ': raise ValueError("no smiley j")
    j, w = struct.unpack(">QQ", query('q' + nick))
    if j == 0: raise ValueError("j = 0 after j")
    if query('w' + nick) != ':)              ': raise ValueError("no smiley w")
    j, w = struct.unpack(">QQ", query('q' + nick))
    if j == 0: raise ValueError("j = 0 after w")
    if w == 0: raise ValueError("w = 0 after w")

for i in xrange(4096):
    hit()
