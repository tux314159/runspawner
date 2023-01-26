#! /usr/bin/python
from subprocess import *
p = Popen("./shserver", stdin=PIPE, stdout=PIPE)
print(p.stdout.readline())
p.stdin.write(b"echo hi\n")
print(p.stdout.readline())
p.stdin.write(b"echo bye\n")
#print(p.stdout.readline())
p.communicate()
