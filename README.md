TinfoilSub
==========

TinfoilSub allows you to use YouTube's subscribe feature without registering a
Goolge-account. The whole project is ridiculous and maybe pointless, but
functional (hehe) nonetheless.

Features
--------

- Scrapes a list of channel pages for videos
- Displays them on a single local page
- Is sort of REST
- Uses all your CPUs/cores to fetch and parse concurrently

Building
--------

TinfoilSub uses [stack](https://stackage.org).

Build with

```
stack install
```

Usage
-----

TinfoilSub uses a list of channels that are placed inside of `channels`,
a newline-separated list of names. It starts listening on `localhost:3000` and
will display the latest videos uploaded by channels on said list.

