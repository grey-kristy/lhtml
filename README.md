lhtml
=====

SimpLe HTML DSL for erlang


```erlang

table() ->
    html:table([
        html:caption(html:h3("Fruits")),
        html:tr([html:td("Apple"), html:td(5)]),
        html:tr([html:td('Oranges'), html:td(4)]),
        html:tr(html:td(html:h3("Vegetables"), {colspan, 2})),
        html:tr([html:td('Cucumbers'), html:td(2)])
    ]).

```
