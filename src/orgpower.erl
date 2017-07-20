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
% date  : 2017-07-19

-module(orgpower).
-export([main/1]).

-include("member.hrl").

-define(LIMIT, 100).

add_repo_to_member(Member, Token) ->
    #member{login = Login, repositories = Repos} = Member,
    NewRepos = orgpower_repository:get_repositories(Login, Token, ?LIMIT),
    Member#member{repositories = lists:append(NewRepos, Repos)}.

add_repo_to_members(News, Members, _) when length(Members) =:= 0 ->
    News;

add_repo_to_members(News, Members, Token) ->
    [Member | Remain] = Members,
    add_repo_to_members(
      [add_repo_to_member(Member, Token) | News],
      Remain,
      Token).

main(Args) ->
    inets:start(),
    ssl:start(),
    [Token | OrgName] = Args,
    Members = orgpower_member:get_members(OrgName, Token, ?LIMIT),
    MembersWithRepo = add_repo_to_members([], Members, Token),
    StarSum = orgpower_calc:get_org_star_count(MembersWithRepo),
    io:format("Organization power is ~p.~n", [StarSum]),
    erlang:halt(0).
