# DalmatinerDB Protocol X(-translator)

Official Site: https://dalmatiner.io/

A multi metric and event log protocol translator for use with DalmatinerDB.

## Supported metric formats
Supports Graphite, Metrics2.0, Influx, Prometheus and OpenTSDB

## Supported log formats
Currently only Syslog is supported.

![dalmatiner architecture](http://cdn2.hubspot.net/hubfs/528953/dalmatiner.png "Dalmatiner Architecture")

This protocol translator connects by default to DalmatinerDB Storage on localhost:5555 and DalmatinerDB Metadata (Postgres) on localhost:5432

Requirements: As per the diagram you will need both DalmatinerDB and Postgres running.

# Metric Listener Configuration

Settings are configured in dpx.conf

## DQE Indexer backend

Firstly, it is important to configure the dqe indexer module that you intend to use.

For example, for the Postgres Indexer, configure your dpx.conf as follows:
```
idx.backend = dqe_idx_pg
```

## Graphite

Enable the Graphite listener with the following config lines.

```
listeners.dp_graphite.bucket = graphite
listeners.dp_graphite.port = 2003
```

### Note (Graphite Optional Extras):

You can optionally include tags on Graphite metrics by including an = sign in a segment. This segment will be handled as a key value pair for a tag.

Ordering is important! Example:

`a.tag1=value.tag2=value` and `a.tag2=value.tag1=value` are different metrics!

Metadata is supported by inserting an 'empty'. Example:

`segment.so a.tag=value..metadata=value` and `a.tag=value..metadata=other_value` will end up in the same metric just change the value of metadata.

When using tags the base of the metric has to be provided first, followed by tags, so in the example above a would be the base metric.

## InfluxDB

Enable the Influx listener with the following config lines.

```
listeners.dp_influx.bucket = influx
listeners.dp_influx.bucket = http
listeners.dp_influx.port = 8086
```

## Metrics 2.0

Enable the Metrics 2.0 listener with the following config lines.

```
listeners.dp_metrics2.bucket = metrics2.0
listeners.dp_metrics2.protocol = tcp
listeners.dp_metrics2.port = 2004
```

The metrics 2.0 protocol us fully supported, all metrics 2.0 metrics use the base metric `metric` with the data fully in tags.

## Prometheus Scrapper

Enable the Prometheus scraper with the following config lines.
```
prometheus_scrapers.node_exporter.bucket = prom
prometheus_scrapers.node_exporter.url = http://localhost:9100/metrics
prometheus_scrapers.node_exporter.frequency = 10000
```

## Prometheus Remote Wrtiter

To enabler the Prometheus remote write API:
```
listeners.dp_prom_writer.bucket = promwriter
listeners.dp_prom_writer.port = 1234
listeners.dp_prom_writer.protocol = http
```

And configure the remote written in the Prometheus config:
```yaml
remote_writer:
  url: "http://<host>:1234/receive"
```


## OpenTSDB

Enable the OpenTSDB listener with the following config lines.

```
listeners.dp_otsdb.bucket = OpenTSDB
listeners.dp_otsdb.protocol = tcp
listeners.dp_otsdb.port = 4242
```

# Log Listener Configuration

Settings are configured in dpx.conf 

## Syslog

Enable the OpenTSDB listener with the following config lines.

```
listeners.dp_syslog.bucket = syslog
listeners.dp_syslog.port = 514
listeners.dp_syslog.protocol = udp
```

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
