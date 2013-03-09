Telegrams Examples
==================

To run the examples you need Python 2.7:

    $ cd telegrams
    $ rel/telegrams/bin/telegrams start
    $ pip install -r examples/requirements.txt

Now start the producer:

    $ cd telegrams/examples
    $ python producer.py mychannel

then, in another terminal start the consumer:

    $ cd telegrams/examples
    $ python consumer.py mychannel

You can minitor the raw event stream with curl:

    $ curl -N http://localhost:8353/mychannel

