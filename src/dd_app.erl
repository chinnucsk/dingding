%% Copyright 2012 Gert Meulyzer

%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
-module(dd_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, start/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%% API
%% this function is added for an easy CLI start (erl -s dd)
start() ->
    ensure_started(crypto),
    ensure_started(public_key),
    ensure_started(sasl),
    ensure_started(ssl),
    ensure_started(inets),
    ensure_started(dd).

ensure_started(App) ->
    case application:start(App) of
        ok -> ok;
        {error, {already_started, App}} -> ok
    end.

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    inets:start(),
    ssl:start(),
    reloader:start(),
    dd_sup:start_link().

stop(_State) ->
    ok.

-ifdef(TEST).

simple_test() ->
    ok = application:start(dd),
    ?assertNot(undefined == whereis(dd_sup)).

-endif.
