About CLSS
----------
CLSS is a DOM traversal engine based on CSS selectors. It makes use of the [Plump-DOM](https://shinmera.github.io/plump/) and is used by [lQuery](https://shinmera.github.io/lquery/).

How To
------
Load CLSS through Quicklisp or ASDF:

    (ql:quickload :clss)

Using a standard CSS selector you can retrieve a vector of nodes from the DOM:

    (clss:select "img" (plump:parse "<div><p>A beautiful image: <img src="//example.com/image.png" alt="image" /></p></div>"))

CLSS implements [Level 3 selectors](http://www.w3.org/TR/css3-selectors/) and offers most of the features from the spec. Some things were left out as they make no sense outside a CSS context.
As Plump supports XML as well as HTML, it also includes special handling for a few nodes that are not elements and are thus not reachable by standard CSS selectors. In order to solve this problem, CLSS adds an extra operator, the `^` caret. The caret is followed by a Plump-DOM class-name and will then match any elements that conform to a `typep` test against it.

    (clss:select "^CDATA" (plump:parse "<foo><![CDATA[bar]]></foo>"))

CSS selectors in themselves also don't support XML namespaces due to the ambiguity arising with pseudo-selectors. CLSS solves this by interpreting a double colon as a name. Thus, a tag with the name of `foo:bar` is selected by `foo::bar`.

CLSS attempts to be a fast engine and various parts of it have been tuned for this purpose, which limits the extensibility of CLSS a bit. However, it is still possible to f.e. programmatically construct a selector.

Extending CLSS
--------------
Using `define-pseudo-selector` you can add your own extensions to CLSS:

    (clss:define-pseudo-selector outside-link (node)
      (let ((href (plump:attribute node "href")))
        (and href (cl-ppcre:scan "^(http|https)://" href))))

    (clss:select "a:outside-link" (plump:parse "<foo><a href=\"/baloney\"/><a href=\"http://example.com\"/></foo>"))
