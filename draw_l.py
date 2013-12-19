# playing with L-systems
import turtle

turtle.speed(10)

SIZE = [10]
INITIAL_POS = [0]

def draw_l(word):
    turtle.up()
    turtle.clear()
    turtle.setposition(0, 0)
    turtle.setheading(0)
    turtle.bk(INITIAL_POS[0])
    turtle.down()
    turtle.st()
    stack = []
    for char in word:
        if char == '0':
            turtle.fd(SIZE[0])
        if char == '1':
            turtle.fd(SIZE[0])
        if char == '[':
            stack.append((turtle.position(), turtle.heading()))
            turtle.lt(45)
        if char == ']':
            position, heading = stack.pop()
            turtle.up()
            turtle.setposition(position)
            turtle.setheading(heading)
            turtle.rt(45)
            turtle.down()
    turtle.ht()

def iterate_l(word):
    return "".join(map(replace_l.__getitem__, word))

replace_l = {
    '1': '11',
    '0': '1[0]0',
    '[': '[',
    ']': ']',
}

def repeatedly(f, v, n):
    while n > 0:
        v = f(v)
        n -= 1
    return v
