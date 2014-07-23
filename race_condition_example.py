import thread
import threading
import sys
import dis
import os

done = threading.Semaphore(0)

A = [0]

poison = [0]

def i_have_a_race_condition(a):
    while 1:
        if poison[0]:
            done.release()
            return

        old = a[0]
        a[0] = old + 1
        new = a[0]
        if new != (old + 1):
            sys.stdout.write("Oh no! Saw %d instead of %d.\n" % (new, old + 1))
            poison[0] = 1

# this has no semantic effect except to make thread-switching
# happen more often, which a) makes the program run much slower,
# and b) makes interleaving bugs more likely to show up
sys.setcheckinterval(5)

thread.start_new_thread(i_have_a_race_condition, (A,))
thread.start_new_thread(i_have_a_race_condition, (A,))
done.acquire()
done.acquire()

sys.stdout.write("Wellp.\n")
