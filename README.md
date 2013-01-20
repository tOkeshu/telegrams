Telegrams
=========

Telegrams is a clustered message bus.

## Getting Started

### Requirements

  * [Erlang](http://www.erlang.org/download.html) (R15B01 or later)
  * [Rebar](https://github.com/basho/rebar)

You will need `rebar` installed in your `$PATH`.

Please see the [rebar repository](https://github.com/basho/rebar) for
downloading and building instructions.

### Install the project

    $ git clone https://github.com/tOkeshu/telegrams.git
    $ cd telegrams
    $ make app

These commands should pull the Erlang dependencies via Rebar and build
a release.

### Start a node

    $ ./rel/telegrams/bin/telegrams start
    $ curl http://localhost:8080/my/chan/nel -N

in another terminal

    $ curl -X POST http://localhost:8080/my/chan/nel --data-binary "hello"
    $ curl -X POST http://localhost:8080/my/chan/nel --data-binary " world!"

and see the data appears in the first terminal.

## License

Telegrams are released under the terms of the
[GNU Affero General Public License v3](http://www.gnu.org/licenses/agpl-3.0.html)
or later.

