% Copyright 2017 Yoshihiro Tanaka
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
% % Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
%
% Author: Yoshihiro Tanaka <contact@cordea.jp>
% date  : 2017-07-20

-module(orgpower_member).
-export([get_members/3]).

-include("member.hrl").

graphql_query(Login, LastId, Limit) when LastId =:= "" ->
    Query = io_lib:format("
    organization(login:\\\"~s\\\") {
        members(first:~p) {
            totalCount
            nodes {
                id
                login
                name
                followers {
                    totalCount
                }
            }
        }
    }", [Login, Limit]),
    orgpower_query:add_query_base(Query);

graphql_query(Login, LastId, Limit) ->
    Query = io_lib:format("
    organization(login:\\\"~s\\\") {
        members(first:~p, after:\\\"~s\\\") {
            totalCount
            nodes {
                id
                login
                name
                followers {
                    totalCount
                }
            }
        }
    }", [Login, Limit, LastId]),
    orgpower_query:add_query_base(Query).

parse_members(JsonList, Members, LastId) when length(JsonList) =:= 0 ->
    {Members, LastId};

parse_members(JsonList, Members, _) ->
    [Json | Remain] = JsonList,
    Id = binary_to_list(proplists:get_value(<<"id">>, Json)),
    Login = binary_to_list(proplists:get_value(<<"login">>, Json)),
    Name = binary_to_list(proplists:get_value(<<"name">>, Json)),
    Follower = proplists:get_value(<<"followers">>, Json),
    FollowerCount = proplists:get_value(<<"totalCount">>, Follower),
    parse_members(
      Remain,
      [#member{id = Id, login = Login, name = Name, followerCount = FollowerCount} | Members],
      Id
    ).

parse_member(Json) ->
    Data = proplists:get_value(<<"data">>, Json),
    Org = proplists:get_value(<<"organization">>, Data),
    Members = proplists:get_value(<<"members">>, Org),
    TotalCount = proplists:get_value(<<"totalCount">>, Members),
    Nodes = proplists:get_value(<<"nodes">>, Members),
    {ParsedMembers, Id} = parse_members(Nodes, [], ""),
    {ParsedMembers, Id, TotalCount}.

get_members(Members, _, _, Count, _, _) when Count =:= 0 ->
    Members;

get_members(Members, OrgName, Token, Count, Limit, LastId) when Count =:= -1, length(Members) =:= 0 ->
    Query = graphql_query(OrgName, LastId, Limit),
    Json = orgpower_request:post(Token, Query),
    {NewMembers, Id, TotalCount} = parse_member(Json),
    get_members(
      lists:append(NewMembers, Members),
      OrgName,
      Token,
      TotalCount - length(NewMembers),
      Limit,
      Id
    );

get_members(Members, OrgName, Token, Count, Limit, LastId) ->
    Query = graphql_query(OrgName, LastId, Limit),
    Json = orgpower_request:post(Token, Query),
    {NewMembers, Id, _} = parse_member(Json),
    get_members(
      lists:append(NewMembers, Members),
      OrgName,
      Token,
      Count - length(NewMembers),
      Limit,
      Id
    ).

get_members(OrgName, Token, Limit) ->
    get_members([], OrgName, Token, -1, Limit, "").
