import sys


def abomination():
    # get the stack frame of the function which called me
    fr = sys._getframe(1)
    # create a global variable visible to caller
    fr.f_globals['z'] = "z wasn't defined, but it is now."
    # spy on caller's local variable
    print ("Supposedly hidden value of x: %s" % (fr.f_locals['x'],))
    # spy on caller's name
    return "I was called by a function called %r" % (fr.f_code.co_name,)


def victim():
    x = "Some hidden secret, never to be revealed."
    y = abomination()
    print ("y = %s" % (y,))
    print ("z = %s" % (z,))  # note the lack of 'z' in scope!

if __name__ == '__main__':
    victim()
