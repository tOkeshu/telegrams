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

    $ ./rel/telegrams/bin/telegrams start # listening on 8353
    $ curl http://localhost:8353/my/chan/nel -N

in another terminal

    $ curl -X POST http://localhost:8353/my/chan/nel --data-binary "hello"
    $ curl -X POST http://localhost:8353/my/chan/nel --data-binary " world!"

and see the data appears in the first terminal.

## Cluster

Telegrams can work as a cluster. For this you need to configure multiple nodes.

### Same machine

For tests purpose it could be relevant to launch multiple nodes on the same machine.
To do this, edit `vm.args` and `sys.config` in `rel/files`:

    $ vim rel/files/vm.args rel/files/sys.config
    $ grep sname rel/files/vm.args
    -sname telegrams1@host1
    $ grep port rel/files/sys.config
     {telegrams, [{port, 8353}]},

Build the application and copy the release to a specific directory:

    $ make app && cp rel/telegrams rel/telegrams1

Do the same for the second node but with a different `sname` and `port`:

    $ vim rel/files/vm.args rel/files/sys.config
    $ grep sname rel/files/vm.args
    -sname telegrams2@host1
    $ grep port rel/files/sys.config
     {telegrams, [{port, 8354}]},
    $ make app && cp rel/telegrams rel/telegrams2

Now start the two nodes and listen from one of them:

    $ ./rel/telegrams1/bin/telegrams start
    $ ./rel/telegrams2/bin/telegrams start
    $ curl http://localhost:8353/my/chan/nel -N # also works with 8354

in another terminal

    $ curl -X POST http://localhost:8353/my/chan/nel --data-binary "from first node "
    $ curl -X POST http://localhost:8354/my/chan/nel --data-binary "from second node"

Now play with it, grab a beer and chill...

### Different machines

With different machines, you need to edit `hosts` in `rel/files/hosts`:

    $ cat rel/files/hosts
    'host1'.
    'host2'.

    $ # don't forget the newline at the end

Then you need to reproduce the same steps as for one machine, except
you don't need to create a specific directory and can use the same
port everywhere.

## License

Telegrams are released under the terms of the
[GNU Affero General Public License v3](http://www.gnu.org/licenses/agpl-3.0.html)
or later.

