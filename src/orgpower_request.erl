% Copyright 2017 Yoshihiro Tanaka
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
%
% Author: Yoshihiro Tanaka <contact@cordea.jp>
% date  : 2017-07-20

-module(orgpower_request).
-export([post/2]).

-define(ENDPOINT, "https://api.github.com/graphql").
-define(USER_AGENT, "Orgpower").

request_header(Token) ->
    {"Authorization", io_lib:format("bearer ~s", [Token])}.

for_post(Query) ->
    lists:concat(string:replace(Query, "\n", "", all)).

request(Token, Query) ->
    Header = [
        request_header(Token),
        {"User-Agent", ?USER_AGENT}
    ],
    case httpc:request(post, {?ENDPOINT,
        Header, "application/json", for_post(Query)}, [{ssl, [{verify, 0}]}], [{body_format, binary}]) of
        {ok, {_, _, Body}} -> Body;
        {error, Reason} -> throw(io_lib:format("~p", Reason))
    end.

post(Token, Query) ->
    Body = request(Token, Query),
    jsx:decode(Body).
