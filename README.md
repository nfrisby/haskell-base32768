A Haskell implementation of https://github.com/qntm/base32768.
There's no important innovation here, just a translation into Haskell idioms with some "obvious" optimizations.

# No `decode` (yet?)

For my use case, I don't need `decode`.
Neither would most Haskell use-cases, since the ecosystem strongly favors UTF-8; this codec is most likely used to export `ByteString`s from Haskell.

# Motivation

I came across https://github.com/qntm/base32768 for two reasons: I'm a fan of https://qntm.org, and I was trying to use an interactive JavaScript plotting library with some non-trivial data I was generating outside of JavaScript.

It turns out that's a bit awkward.
In my particular situation, the easiest solution was to serialize the data as an "unencoded" byte stream, and then load it via the Fetch API; see https://developer.mozilla.org/en-US/docs/Web/API/Response/bytes#examples.

```
const theData = new Float64Array(await (await fetch("the-data.bin")).arrayBuffer());
```

This comes at a convenience cost, though.
Due to https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS/Errors/CORSRequestNotHttp#loading_a_local_file, I can no longer simply load my `html` file in the browser; I need to at least run a static server.

```
nix-shell -p static-web-server --run 'static-web-server -p 8000 -a 127.0.0.1 -d .'
```

OK, now it works, and in a relatively straight-forward way.

The final hurdle, though, is distribution.
The web server requirement makes it inconvenient to distribute my HTML file to my colleagues.
I want them to use the interactive features that the JavaScript enables, it's good for them and makes it easier for them to have insights that will benefit the work.
So I'd rather not force them to setup a web server in order to view it!

The web server is only required by the Fetch API.
If I directly encode the data in a JavaScript source file, then the browser does the right thing without requiring any explicit fetch requests.

```
<script src="the-data.js"></script>
<script src="the-plotting-logic.js"></script>
```

And if I inline those scripts, then I can simply share a single HTML file with my colleagues (using the recommended CDN links for the JavaScript plotting library), and it'll work for them immediately.

There are plenty of ways to encode the data in JavaScript syntax, but only one way felt satisfying: https://github.com/qntm/base32768.
That's because of two key facts: JavaScript doesn't have array/bytestring literals and browsers encode JavaScript literal strings as UTF-16 (regardless of what the source file's encoding is).
So I need to encode my data as a UTF-16 string in order to avoid it being re-encoded by the browser's JavaScript interpreter and to ensure it's efficiently bitpacked.

The space-efficiency requirement disqualifies the well-known `base64` alternative --- see the comparison table at https://github.com/qntm/base32768.

Hence this Haskell package, since my data happens to originate in Haskell.

# Testing

Test by loading `test-src/test.html` in your browser and following its instructions.
