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

-module(orgpower_repository).
-export([get_repositories/3]).

-include("repository.hrl").

graphql_query(Login, LastId, Limit) when LastId =:= "" ->
    Query = io_lib:format("
    user(login:\\\"~s\\\") {
        repositories(first:~p) {
            totalCount
            nodes {
                id
                name
                stargazers {
                    totalCount
                }
            }
        }
    }", [Login, Limit]),
    orgpower_query:add_query_base(Query);

graphql_query(Login, LastId, Limit) ->
    Query = io_lib:format("
    user(login:\\\"~s\\\") {
        repositories(first:~p, after:\\\"~s\\\") {
            totalCount
            nodes {
                id
                name
                stargazers {
                    totalCount
                }
            }
        }
    }", [Login, Limit, LastId]),
    orgpower_query:add_query_base(Query).

parse_repositories(JsonList, Repos, LastId) when length(JsonList) =:= 0 ->
    {Repos, LastId};

parse_repositories(JsonList, Repos, _) ->
    [Json | Remain] = JsonList,
    Id = binary_to_list(proplists:get_value(<<"id">>, Json)),
    Name = binary_to_list(proplists:get_value(<<"name">>, Json)),
    Stargazers = proplists:get_value(<<"stargazers">>, Json),
    TotalCount = proplists:get_value(<<"totalCount">>, Stargazers),
    parse_repositories(
      Remain,
      [#repository{id = Id, name = Name, starCount = TotalCount} | Repos],
      Id
    ).

parse_repository(Json) ->
    Data = proplists:get_value(<<"data">>, Json),
    User = proplists:get_value(<<"user">>, Data),
    Repositories = proplists:get_value(<<"repositories">>, User),
    TotalCount = proplists:get_value(<<"totalCount">>, Repositories),
    Nodes = proplists:get_value(<<"nodes">>, Repositories),
    {ParsedRepos, Id} = parse_repositories(Nodes, [], ""),
    {ParsedRepos, Id, TotalCount}.

get_repositories(Repos, _, _, Count, _, _) when Count =:= 0 ->
    Repos;

get_repositories(Repos, UserName, Token, Count, Limit, LastId) when Count =:= -1, length(Repos) =:= 0 ->
    Query = graphql_query(UserName, LastId, Limit),
    Json = orgpower_request:post(Token, Query),
    {NewRepos, Id, TotalCount} = parse_repository(Json),
    get_repositories(
      lists:append(NewRepos, Repos),
      UserName,
      Token,
      TotalCount - length(NewRepos),
      Limit,
      Id);

get_repositories(Repos, UserName, Token, Count, Limit, LastId) ->
    Query = graphql_query(UserName, LastId, Limit),
    Json = orgpower_request:post(Token, Query),
    {NewRepos, Id, _} = parse_repository(Json),
    get_repositories(
      lists:append(NewRepos, Repos),
      UserName,
      Token,
      Count - length(NewRepos),
      Limit,
      Id).

get_repositories(UserName, Token, Limit) ->
    get_repositories([], UserName, Token, -1, Limit, "").
