TinfoilSub
==========

TinfoilSub allows you to use YouTube's subscribe feature without registering a
Goolge-account. The whole project is ridiculous and maybe pointless, but
functional (hehe) nonetheless.

Features
--------

- Scrapes a list of channel pages for videos
- Black-/whitelist filters for video-titles
- Displays them on a single local page
- Is sort of REST
- Uses all your CPUs/cores to fetch and parse concurrently
- Includes a minimal video viewing page without comments and the like

Building
--------

TinfoilSub uses [stack](https://stackage.org).

Build with

```
stack install
```

Usage
-----

Run TinfoilSub like this, specifying the path to a config file as the first and
only argument:

```
tinfoilsub <config>
```

The config file uses the following syntax:

```
channelname [+ keyword [keyword ..]] [- keyword [keyword ..]] # Comment
```

A plus sign starts a whitelist filter that ends with either a plus or minus
sign or a newline. A minus sign starts a blacklist filter. Filters can be
combined at will and will be combined with "AND". Plus and minus signs need to
be space-separated from surrounding words. A filter can use multiple keywords.
Keywords are case-insensitive. If channels are specified multiple times (e.g.
for multiple different whitelists) each video will only show up once. Comments
can be appended to feeds or on their own lines, empty lines are permitted as
well.

