# grammar for parsing

# e -> e0 '#'

# e0 -> e1 (('+'|'-') e1)*
# e1 -> e2 (('*'|'/'|'%') e2)*
# e2 -> (('+','-')* e2)
# e2 -> e3 ('**' e2)*
# e3 == tn

# tn -> number
# tn -> function '(' ')'
# tn -> function '(' l0 ')'
# tn -> constant
# tn -> '(' e0 ')'
# l0 -> e0
# l0 -> e0 ',' l0

import math
import random

functions = dict([(name.upper(), getattr(math, name)) for name in
                  "acos asin atan atan2 ceil cos cosh degrees exp fabs floor fmod frexp hypot ldexp log log10 modf pow radians sin sinh sqrt tan tanh".split(" ")])
functions['RANDINT'] = random.randint
functions['COMPLEX'] = complex
consts = dict(E=math.e, PI=math.pi, I=complex(0, 1), J=complex(0, 1))

def sorted(it, *args, **kwargs):
    l = list(it)
    l.sort(*args, **kwargs)
    return l

def calc(expr):
    if expr == "help":
        h = ["Calculator. Numbers like 100, 100.0, 1e2, 1.0e2 all ",
             "work. The operators +, -, *, /, %, ** are available. The ",
             "constants available are: "]
        h.append(", ".join(sorted(consts.iterkeys())))
        h.append(". The functions available are: ")
        h.append(", ".join(sorted(functions.iterkeys())))
        return "".join(h)

    x = [c for c in expr if not c.isspace()]
    x.append('#')
    x = "".join(x)

    p = [0]

    def require(b):
        if not b:
            raise Exception("parse error, unexpected %r" % peek())
    # peek() and nom() are now doing lexing instead of just characters
    def nom():
        if peek() == '**':
            p[0] = p[0] + 1
        p[0] = p[0] + 1
    def peek():
        c = x[p[0]]
        if c == '*' and x[p[0]+1] == '*':
            return '**'
        return c

    def e():
        v = e0()
        require(peek() == '#')
        return v

    def e0():
        v = e1()
        while peek() in '+-':
            o = peek()
            nom()
            v2 = e1()
            if o == '+':
                v = v + v2
            else:
                v = v - v2
        return v

    def e1():
        v = e2()
        while peek() in '*/%':
            o = peek()
            nom()
            v2 = e2()
            if o == '*':
                v = v * v2
            elif o == '/':
                v = v / v2
            else:
                v = v % v2
        return v

    def e2():
        z = 1
        while peek() in "+-":
            o = peek()
            nom()
            if o == '-':
                z *= -1
        # cheat, dropping out of the above while loop is
        # equivalent to returning z*e2() in the grammar.
        v = tn()
        while peek() == '**':
            nom()
            v2 = e2()
            v = v ** v2
        return z*v

    def tn():
        tn_states = ('.e', '+-', '', '', 'e')
        if peek().isdigit():
            v = ''
            state = 0
            while peek().isdigit() or (peek() in tn_states[state]):
                if peek() == 'e':
                    state = 1
                # after eating e, single following char may be + or -.
                elif state == 1:
                    state = 2
                elif peek() == '.':
                    state = 3
                # after eating ., single following char must be digit
                elif state == 3:
                    state = 4
                v += peek()
                nom()
            v = float(v)
            return v
        if peek().isalpha():
            fn = ''
            while peek().isalpha():
                fn += peek()
                nom()
            fn = fn.upper()
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
            return functions[fn](*v)
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

    return e()
