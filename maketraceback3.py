from types import CodeType

def make_raiser(name, lineno, filename, type_to_raise):
    def raises():
        raise type_to_raise()
    co = raises.__code__
    raises.__name__ = name
    raises.__code__ = CodeType(
        co.co_argcount,
        co.co_kwonlyargcount,
        co.co_nlocals,
        co.co_stacksize,
        co.co_flags,
        co.co_code,
        co.co_consts,
        co.co_names,
        co.co_varnames,
        filename,
        name,
        lineno - 1,
        co.co_lnotab,
        co.co_freevars,
        co.co_cellvars,
    )
    return raises

foo = make_raiser('foo', 3, 'foo.rs', lambda: IndexError(0))
bar = make_raiser('bar', 4, 'bar.rs', foo)
bar()

# Output:
# $ python3 maketraceback3.py 
# Traceback (most recent call last):
#   File "maketraceback3.py", line 29, in <module>
#     bar()
#   File "bar.rs", line 4, in bar
#   File "foo.rs", line 3, in foo
# IndexError: 0
