-module(example).

-export([test/0]).

html() ->
    html:html([
        html:head([
            html:title("Example"),
            html:hlink([{rel, stylesheet}, {type, 'text/css'}, {href, '/css/main.css'}])
        ]),
        html:body(body())
    ]).

body() ->
    html:hdiv([header(), table()], {class, container}).

header() ->
    html:h1(html:a("https://github.com/grey-kristy/lhtml", "Hello!")).

table() ->
    html:table([
        html:caption(html:h3("Fruits")),
        html:tr([html:td("Apple"), html:td(5)]),
        html:tr([html:td('Oranges'), html:td(4)]),
        html:tr(html:td(html:h3("Vegetables"), {colspan, 2})),
        html:tr([html:td('Cucumbers'), html:td(2)])
    ]).

test() ->
    io:format("~n~ts~n", [html()]).
