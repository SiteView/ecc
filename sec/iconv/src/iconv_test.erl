-module(iconv_test).
-export([test/0]).

test() -> 
    iconv:start(),
    Str = iconv:convert("utf-8", "gbk","中华人民共和国"),
    {iconv:convert("gbk","utf-8",Str),Str}.