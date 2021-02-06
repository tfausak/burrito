# Burrito

[![CI](https://github.com/tfausak/burrito/workflows/CI/badge.svg)](https://github.com/tfausak/burrito/actions/new)
[![Hackage](https://img.shields.io/hackage/v/burrito)](https://hackage.haskell.org/package/burrito)
[![Stackage](https://www.stackage.org/package/burrito/badge/nightly?label=stackage)](https://www.stackage.org/package/burrito)

Burrito is a Haskell library for parsing and rendering URI templates.

According to [RFC 6570](https://tools.ietf.org/html/rfc6570): "A URI Template
is a compact sequence of characters for describing a range of Uniform Resource
Identifiers through variable expansion." Burrito implements URI templates
according to the specification in that RFC.

The term "uniform resource identifiers" (URI) is often used interchangeably
with other related terms like "internationalized resource identifier" (IRI),
"uniform resource locator" (URL), and "uniform resource name" (URN). Burrito
can be used for all of these. If you want to get technical, its input must be a
valid IRI and its output will be a valid URI or URN.

Although Burrito is primarily intended to be used with HTTP and HTTPS URIs, it
should work with other schemes as well.
