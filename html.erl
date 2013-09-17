-module(html).

-export([html/2, html/1, head/1, title/1, body/2, body/1, hlink/1]).
-export([h1/1, h1/2, h2/1, h2/2, h3/1, h3/2]).
-export([p/1, p/2, hdiv/1, hdiv/2, span/1, span/2, footer/1, footer/2]).
-export([table/1, table/2, caption/1, caption/2, th/1, th/2, td/1, td/2, tr/1, tr/2]).
-export([img/1, img/2, a/2, a/3]).


tag(Tag, Msg, Opts) when is_integer(Msg) -> tag(Tag, integer_to_list(Msg), Opts);
tag(Tag, Msg, Opts) when is_number(Msg) -> tag(Tag, io_lib:format("~g",[Msg]), Opts);

tag(Tag, Msg, Opts) ->
    case is_list(Opts) of
        true -> Adds = lists:map(fun format_opt/1, Opts);
        false -> Adds = format_opt(Opts)
    end,
    case tag_type(Tag) of
        void ->
            lists:flatten(io_lib:format("<~p~ts/>~n", [Tag, lists:flatten(Adds)]));
        hdiv ->
            lists:flatten(io_lib:format("<~s~ts>~ts</~s>~n", [Tag, lists:flatten(Adds), Msg, Tag]));
        _ ->
            lists:flatten(io_lib:format("<~p~ts>~ts</~p>~n", [Tag, lists:flatten(Adds), Msg, Tag]))
    end.

format_opt({Key, Val}) when is_integer(Val) -> io_lib:format(" ~p=\"~p\"", [Key, Val]);
format_opt({Key, Val}) -> io_lib:format(" ~p=\"~ts\"", [Key, Val]).

tag_type('div') -> hdiv;
tag_type(link) -> void;
tag_type(img) -> void;
tag_type(_) -> full.


html(Msg) -> tag(html, Msg, []).
html(Msg, Opts) -> tag(html, Msg, Opts).
head(Msg) -> tag(head, Msg, []).
title(Msg) -> tag(title, Msg, []).
hlink(Opts) -> tag(link, "", Opts).

body(Msg) -> tag(body, Msg, []).
body(Msg, Opts) -> tag(body, Msg, Opts).

h1(Msg) -> tag(h1, Msg, []).
h1(Msg, Opts) -> tag(h1, Msg, Opts).

h2(Msg) -> tag(h2, Msg, []).
h2(Msg, Opts) -> tag(h2, Msg, Opts).

h3(Msg) -> tag(h3, Msg, []).
h3(Msg, Opts) -> tag(h3, Msg, Opts).

p(Msg) -> tag(p, Msg, []).
p(Msg, Opts) -> tag(p, Msg, Opts).

hdiv(Msg) -> tag('div', Msg, []).
hdiv(Msg, Opts) -> tag('div', Msg, Opts).

span(Msg) -> tag(span, Msg, []).
span(Msg, Opts) -> tag(span, Msg, Opts).

footer(Msg) -> tag(footer, Msg, []).
footer(Msg, Opts) -> tag(footer, Msg, Opts).

table(Msg) -> tag(table, Msg, []).
table(Msg, Opts) -> tag(table, Msg, Opts).

caption(Msg) -> tag(caption, Msg, []).
caption(Msg, Opts) -> tag(caption, Msg, Opts).

th(Msg) -> tag(th, Msg, []).
th(Msg, Opts) -> tag(th, Msg, Opts).

tr(Msg) -> tag(tr, Msg, []).
tr(Msg, Opts) -> tag(tr, Msg, Opts).

td(Msg) -> tag(td, Msg, []).
td(Msg, Opts) -> tag(td, Msg, Opts).

img(Src) -> img(Src, []).
img(Src, Opts) -> tag(img, "", [{src, Src}] ++ Opts).

a(Href, Msg) -> a(Href, Msg, []).
a(Href, Msg, Opts) -> tag(a, Msg, [{href, Href}] ++ Opts).
