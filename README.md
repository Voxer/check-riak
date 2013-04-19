check-riak
==========

A script written and used by [Voxer][voxer] to check [Riak][riak] health
on [SmartOS][smartos]

- [Install](#install)
- [Usage](#usage)
- [Example](#example)
- [Nagios](#nagios)
- [Notes](#notes)
- [License (MIT)](#license)

<a name="install" />

Install
-------

**Note:** Only tested on [SmartOS][smartos]

You can run `check-riak` directly out of the git repository.  To install
it globally on the system, run:

    make install

and ensure `/opt/local/bin` is in your PATH

To remove `check-riak`, run

    make uninstall

<a name="usage" />

Usage
-----

    usage: check-riak [ options ] [ check ]

    run this script with no options to get an overview of riak health
    on a system.

    give an optional parameter to only check one portion of the system.

    examples
      check-riak                # full system health overview
      check-riak compaction     # only check for compaction errors
      check-riak -n compaction  # check for compaction, output suitable for nagios

    options
      -a              run all checks
      -c <config>     the riak config file to use, defaults to /opt/local/etc/riak/app.config
      -d <dir>        the database directory, defaults to /var/db/riak/leveldb
      -h              print this message and exit
      -H <host>       the riak host, defaults to localhost
      -n              less pretty output suitable for a nagios check
      -p <port>       the riak port, defaults to 8098
      -s <svc>        the service fmri to use for SMF, defaults to riak
      -t <seconds>    the timeout, in seconds, when curl'ing riak, defaults to 10
      -W <bytes>      the warning threshold for rss in bytes, defaults to 50000000000 (50g)
      -C <bytes>      the critical threshold for rss in bytes, defaults to 60000000000 (60g)

    optional arguments
      compaction      check for compaction errors
      config          show the first line of the riak config
      oktostart       check if riak is ok to start
      ping            check riak ping using /ping
      process         parse ps for the running process
      profile         profile beam.smp with DTrace
      riakping        check riak ping locally
      rss             check riak rss
      service         show svcs output for riak
      singleton       see if this machine is a single cluster
      stats           check riak stats (riak_kv_vnodes_running)
      system          check riak system health

<a name="example" />

Example
-------

Run `check-riak` with no arguments to see health of a node

    $ sudo check-riak
                                Riak Cluster
    %% Test Cluster

                            Checking Service
    STATE          STIME    FMRI
    online         13:24:29 svc:/application/riak:default

                            Checking Process
        riak 58798 58796   0 13:24:22 pts/8       0:08 /opt/local/lib/riak/erts-5.9.1/bin/beam.smp -K true -A 64 -W w -- -root /opt/lo

                               System Health
    pid 58798: 65,672K rss (279 open files)

                          Checking Riak Ping
    pong

                    Checking Riak Stats Ping
    OK

                          Checking Singleton
    ok: 16 running of 64 total (25%)

                  Checking Compaction Errors
    -------- Compaction errors found! --------
    The following steps will resolve them
    ** stop the process before running!! **

    # Run the following command(s) one-by-one
    # (safe to run multiple commands at the same time to saturate disks)
    /opt/local/lib/riak/erts-5.9.1/bin/escript "./lib/lvlfix.erl" "/var/db/riak/leveldb/356811923176489970264571492362373784095686656000"

There's a lot of output, so let's go through it section by section.

#### `Riak Cluster`

This section is very simple, it prints out the first line (`head -1`) of the
Riak `app.config` file.

At Voxer, we have multiple Riak clusters and we put the name of the cluster as
a comment on the first line of the config, so this helps us identify which
cluster we are currently checking.

#### `Checking Service`

This will check SMF on machines with `svcs` to show the current state
of the service.

If your machine lacks SMF and the `svcs` binary, this check will be skipped

#### `Checking Process`

Just a simple parsing of `ps` for the riak beam process

This is useful for situations where SMF shows the process as disabled or in
maintenance mode, but the process is still up and running.

#### `System Health`

Shows the PID, RSS, and # of open files for Riak.

This is useful in situations where SMF shows the process as enabled, the
beam process is found in `ps`, but the `/proc` structure cannot be accessed.
This is usually a sign that Riak has crashed and is writing a core file.

#### `Checking Riak Ping`

This is the output of `riak ping`, it should say `pong`

#### `Checking Riak Stats Ping`

This is the output of a GET request to riak at `/ping`, it should say `OK`

#### `Checking Singleton`

This checks the number of running VNodes for a single riak process, and compares it
to the `ring_creation_size` found in the `app.config` file.

If the number of VNodes running is greater than or equal to the
`ring_creation_size`, it is considered critical.  At Voxer, we don't have any
singleton clusters; all of our Riak clusters have more than 1 node in them.  If
this check ever becomes critical, it means a node is running Riak, but is not
part of cluster.  If it remains in the load-balancer it could potentially
result in data corruption or data loss.

In order to retrieve the number of VNodes running, the stats interface `/stats`
is curl'd and [jsontool][jsontool] is used to extract the metric
`riak_kv_vnodes_running`. `ring_creation_size` is gathered by grepping and
awking the `app.config` file.

#### `Checking Compaction Errors`

This check looks for compaction errors for the data on the filesystem.

If there are any compaction errors found, this tool will print the steps required
to resolve the errors.  The `lvlfix.erl` script used for fixing the errors
is included with this tool.

---

In the above example, you can see that every check has passed except the
check for Compaction Errors.  Let's follow the directions to resolve the
compaction errors.

    $ sudo svcadm disable riak
    $ sudo /opt/local/lib/riak/erts-5.9.1/bin/escript "./lib/lvlfix.erl" "/var/db/riak/leveldb/356811923176489970264571492362373784095686656000"
    ["356811923176489970264571492362373784095686656000"]
    Compactions
    Level  Files Size(MB) Time(sec) Read(MB) Write(MB)
    --------------------------------------------------

Now that the compaction errors have been resolved, let's make sure Riak is OK
to start, running `check-riak` we'll see different output as the service
is not currently running.

    $ sudo check-riak
                                Riak Cluster
    %% Test Cluster

                            Checking Service
    STATE          STIME    FMRI
    disabled       15:05:33 svc:/application/riak:default

                            Checking Process
    not running

                        Checking OK To Start
    All Partitions compacted, OK to start.

                  Checking Compaction Errors
    ok: no compaction errors found

No compaction errors, and Riak is ready to start.

<a name="nagios" />

Nagios
------

Some of the checks support outputting to a nagios-friendly format with the `-n` option.

### rss

    $ sudo check-riak -n rss
    ok: 65,664K rss
    $ echo $?
    0

### ping

    $ sudo check-riak -n ping
    OK
    $ echo $?
    0

### singleton

    $ sudo check-riak -n singleton
    ok: 16 running of 64 total (25%)
    $ echo $?
    0

### compaction

    $ sudo check-riak -n compation
    ok: no compaction errors found
    $ echo $?
    0

<a name="notes" />

Notes
-----

- This script *should* work on other operating systems, though it hasn't been tested by us
- This script assumes the first line of your `app.config` is meaningful
- [jsontool][jsontool] is required for some of the checks to work; SmartOS has this installed by default
- Escalated privileges are required for certain actions, though `sudo` itself isn't necessarily required
- Third-party tools provided to fix compaction and check if riak is OK to start provided by https://gist.github.com/evanmcc/9fd0c3fb0751b55d376b
- Logic to find compaction errors found here https://gist.github.com/gburd/b88aee6da7fee81dc036

<a name="license" />

License
-------

LICENSE - "MIT License"
Copyright (c) 2013 Voxer LLC. All rights reserved.

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

[tools]: https://gist.github.com/evanmcc/9fd0c3fb0751b55d376b
[riak]: http://basho.com/riak/
[smartos]: http://smartos.org/
[voxer]: http://voxer.com/
[jsontool]: https://npmjs.org/package/jsontool
