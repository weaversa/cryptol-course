import cryptol
import sys

def check(c, name, *args):
    result = c.call(name, *args).result()
    if result != True:
        expression = name
        print(name + " failed on '" + expression + "' with arguments");
        for arg in args:
            print(arg)
        sys.exit(1)
