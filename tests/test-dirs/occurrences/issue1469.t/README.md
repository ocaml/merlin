I'm hovering on identifier `a` on the right and expect that
it will highlight the identifier `a` on the left (which is macro
expanded to `(fun a -> ...)`). But instead it highlights something
weird.

Also go-to-definition for `a` is broken.

I'm not sure, is something wrong with my syntax extension or with
lsp-server itself. Please, help!

![Screenshot1](/Screenshot1.png "text")
