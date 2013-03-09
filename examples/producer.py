from sys import stdin, stdout, argv
from signal import signal, SIGINT
import json
import requests


if len(argv) != 2:
    print('Usage: %s <channel>' % argv[0])
    exit(1)

def sigint(signal, frame):
    print
    exit(0)

signal(SIGINT, sigint)

print('Choose a nickname:')
stdout.write('> ')
nick = stdin.readline().strip()
print

print('Type messages, each line will be posted to telegrams')
print('^C to stop')

url = "http://localhost:8353/%s" % argv[1]
while(True):
    stdout.write('<%s> ' % nick)
    message = json.dumps(dict(text=stdin.readline(), nick=nick))
    requests.post(url, data=message)

