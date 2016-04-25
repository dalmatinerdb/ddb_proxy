# ddb_proxy


DalmatinerDB proxy for various metric protocols.

This application allows for using DalmatinerDB as a backend for multiple different metric protocols. Both with simple tree based path as well as with a tagging system.

This means that the Postgres index for DDB is required for this tool.

It should be noted that since DalmatinerDB does only support integers at this point all values are rounded to the next integer.

## Supported Protocols


### Graphite

Graphite is supported as both simple path as well as tagged path. To use tags 
simply include a `=` sign in a segment and this segment will be handled as a key value pair for a tag. Please note that the **order matters** as for graphite this olds true as well, so `a.tag1=value.tag2=value` and `a.tag2=value.tag1=value` are different metrics!

Metric 2.0 like metadata is supported by inserting an 'empty' segment so `a.tag=value..metadata=value` and `a.tag=value..metadata=other_value` will end up in the same metric just change the value of metadata.

When using tags the base of the metric has to be provided first, followed by tags, so in the example above `a` would be the base metric.


### Metrics 2.0

The metrics 2.0 protocol us fully supported, all metrics 2.0 metrics use the base metric `metric` with the data fully in tags.

### Prometheus

Prometheus metrics are supported, however the distinction between different types is not made.

### OpenTSDB

OpenTSDB metrics are supported in their basic form.



Build
-----

```bash
$ rebar3 compile
```

Release
-------

```bash
$ rebar3 release
```