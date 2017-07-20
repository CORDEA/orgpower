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

-module(orgpower_calc).
-export([get_org_star_count/1, get_org_follower_count/1]).

-include("repository.hrl").
-include("member.hrl").

calc_member_star_count(Repos, StarCount) when length(Repos) =:= 0 ->
    StarCount;

calc_member_star_count(Repos, StarCount) ->
    [Repo | Remain] = Repos,
    #repository{starCount = Count} = Repo,
    calc_member_star_count(
      Remain,
      StarCount + Count
    ).

calc_org_star_count(Members, StarCount) when length(Members) =:= 0 ->
    StarCount;

calc_org_star_count(Members, StarCount) ->
    [Member | Remain] = Members,
    #member{repositories = Repos} = Member,
    Count = calc_member_star_count(Repos, 0),
    calc_org_star_count(
      Remain,
      StarCount + Count
    ).

get_org_star_count(Members) ->
    calc_org_star_count(Members, 0).

calc_org_follower_count(Members, FollowerCount) when length(Members) =:= 0 ->
    FollowerCount;

calc_org_follower_count(Members, FollowerCount) ->
    [Member | Remain] = Members,
    #member{followerCount = Count} = Member,
    calc_org_follower_count(
      Remain,
      FollowerCount + Count
    ).

get_org_follower_count(Members) ->
    calc_org_follower_count(Members, 0).
