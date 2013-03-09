from sys import argv, stdout
from signal import signal, SIGINT
import json
import requests
from requests_eventsource import eventsource


if len(argv) != 2:
    print('Usage: %s <channel>' % argv[0])
    exit(1)

def sigint(signal, frame):
    print
    exit(0)

signal(SIGINT, sigint)

url = "http://localhost:8353/%s" % argv[1]
r = requests.get(url, stream=True)
for event in eventsource(r):
    message = json.loads(event.data)
    message = "<%s> %s" % (message['nick'], message['text'])
    stdout.write(message)
