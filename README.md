# pickURL

[![Travis-CI Build Status](https://travis-ci.org/mvkorpel/pickURL.svg?branch=master)](https://travis-ci.org/mvkorpel/pickURL)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/mvkorpel/pickURL?branch=master&svg=true)](https://ci.appveyor.com/project/mvkorpel/pickURL)
[![Coverage Status](https://img.shields.io/codecov/c/github/mvkorpel/pickURL/master.svg)](https://codecov.io/github/mvkorpel/pickURL?branch=master)

Extract URLs and email addresses from text using
[R](https://www.r-project.org/). Actually, all kinds of URIs are
supported, not just URLs. The set of accepted URI schemes can easily
be adjusted.

Leading and trailing punctuation is examined. If it seems that
punctuation is used as delimiters around a URI or that a URI is the
last part of sentence, some trailing punctuation may be
removed. Comma-separated URI lists are split but the heuristics used
for this may fail, as the comma is a valid character in some parts of
a URI. Any technically valid URI is protected from being cut if it is
surrounded by angle brackets (`<http://www.example.org/>`) or double
quotes (`"http://www.example.org/"`). Whitespace is allowed (and
removed) within angle brackets, as long as the URI scheme and the
following `:` are not interrupted by whitespace.

Some (approximate) validation against the [URI
specification](https://www.rfc-editor.org/rfc/rfc3986.txt) is
performed, for example in the host part of the URI. The program also
catches illegal ASCII characters and use of the `%` character for
purposes other than percent-encoding; anything after that, including
the illegal character itself, is not considered a part of the URL. The
program is generally not aware of possible additional rules applying
to URIs following a particular URI scheme. As an exception, the
program knows about the structure of
[mailto](https://www.rfc-editor.org/rfc/rfc6068.txt) URIs.

## Installation

With [devtools](https://github.com/hadley/devtools) already installed,
run the following command in the R console:

```R
devtools::install_github("mvkorpel/pickURL")
```

## Usage

After installing the package, see the help page of function `pick_urls`.

