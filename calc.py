# ll-parser (recursive-descent)
# e0 -> e1
# e0 -> e1 + e0
# e0 -> e1 - e0
# e1 -> e2
# e1 -> e2 * e1
# e1 -> e2 / e1
# e2 -> number
# e2 -> function()
# e2 -> function(l0)
# e2 -> constant
# e2 -> ( e0 )
# l0 -> e0
# l0 -> e0 , l0

import math
import random

def calc(expr):
    x = [c for c in expr if not c.isspace()]
    x.append('$')
    x = "".join(x)

    p = [0]

    def require(b):
        if not b:
            raise Exception("parse error, unexpected %r" % peek())
    def nom():
        p[0] = p[0] + 1
    def peek():
        return x[p[0]]

    functions = dict([(name, getattr(math, name)) for name in
                      "acos asin atan atan2 ceil cos cosh degrees exp fabs floor fmod frexp hypot ldexp log log10 modf radians sin sinh sqrt tan tanh".split(" ")])
    functions['randint'] = random.randint
    functions['complex'] = complex
    functions['pow'] = lambda x, y: x ** y
    consts = dict(e=math.e, pi=math.pi, i=complex(0, 1), j=complex(0, 1))

    def e0():
        v = e1()
        if peek() == '+':
            nom()
            v2 = e0()
            return v + v2
        if peek() == '-':
            nom()
            v2 = e0()
            return v - v2
        return v

    def e1():
        v = e2()
        if peek() == '*':
            nom()
            v2 = e1()
            return v * v2
        if peek() == '/':
            nom()
            v2 = e1()
            return v / v2
        return v

    def e2():
        if peek().isdigit():
            v = ''
            while peek().isdigit() or peek() in '.e':
                v += peek()
                nom()
            v = float(v)
            return v
        if peek().isalpha():
            fn = ''
            while peek().isalpha():
                fn += peek()
                nom()
            if fn in consts:
                return consts[fn]
            require(peek() == '(')
            nom()
            if peek() == ')':
                v = []
            else:
                v = l0()
                require(peek() == ')')
            nom()
            return functions[fn.lower()](*v)
        if peek() == '(':
            nom()
            v = e0()
            require(peek() == ')')
            nom()
            return v
        require(False)

    def l0():
        v = []
        v.append(e0())
        while peek() == ',':
            nom()
            v.append(e0())
        return v

    return e0()
