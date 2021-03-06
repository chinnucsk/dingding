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

%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @doc
%%% General helpers.
%%% @end
%%% Created :  2 Dec 2012 by Gert Meulyzer <@G3rtm on Twitter>

-module(dd_helpers).
-export([get_urls/1, http_get/1, http_head/1, module_exists/1, utc2sec/0, sec2utc/1, get_db_pid/0]).
-export([format_time/0, format_time/1, iso_8601_fmt/1, ppsec2utc/1]).
-export([add_item_to_proplist_value/3, remove_item_from_proplist_value/3]).
-export([diff2now/1, pptimediff/1]).
-export([request_version/2, request_whois/2]).

-spec get_urls(binary()) -> [binary()].
get_urls(Bin) ->
    Parts = binary:split(Bin, <<" ">>, [global, trim]),
    lists:flatten([ case Part of
                        <<"http://", _/binary>> -> Part;
                        <<"https://", _/binary>> -> Part;
                        <<"<http://", _/binary>> -> binary:part(Part, 1, byte_size(Part)-2);
                        <<"<https://", _/binary>> -> binary:part(Part, 1, byte_size(Part)-2);
                       _ -> []
                   end || Part <- Parts ]).

-spec http_get(string()) -> any().
http_get(Url) ->
    httpc:request(get, {Url, [{"User-Agent", "Mozilla/5.0 (X11; Linux x86_64; rv:17.0) Gecko/20100101 Firefox/17.0"}]}, [{autoredirect, true}], []).

-spec http_head(string()) -> any().
http_head(Url) ->
    httpc:request(head, {Url, [{"User-Agent", "Mozilla/5.0 (X11; Linux x86_64; rv:17.0) Gecko/20100101 Firefox/17.0"}]}, [{autoredirect, true}], []).

-spec module_exists(module()) -> boolean().
module_exists(Module) ->
    case is_atom(Module) of
        true ->
            try Module:module_info() of
                _InfoList ->
                    true
            catch
                _:_ ->
                    false
            end;
        false ->
            false
    end.

utc2sec() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()).

sec2utc(Seconds) ->
    calendar:gregorian_seconds_to_datetime(Seconds).

diff2now(Seconds) ->
    Now = dd_helpers:utc2sec(),
    R = Now - Seconds,
    calendar:seconds_to_daystime(R).

%% io_lib:format("~2..0B", [42]),
pptimediff({Days, {Hours, Minutes, Seconds}}) ->
    io_lib:format("~s~s~s~s",[timestr(days, Days), timestr(hours, Hours), timestr(minutes, Minutes), timestr(seconds, Seconds)]).

timestr(seconds, 0) ->
    "exactly";
timestr(_, 0) ->
    "";
timestr(minutes, 1) ->
    "1 minute, ";
timestr(minutes, X) ->
    io_lib:format("~p minutes, ",[X]);
timestr(seconds, 1) ->
    "1 second, ";
timestr(seconds, X) ->
    io_lib:format("~p seconds ",[X]);
timestr(hours, 1) ->
    "1 hour, ";
timestr(hours, X) ->
    io_lib:format("~p hours, ",[X]);
timestr(days, 1) ->
    "a day, ";
timestr(days, X) ->
    io_lib:format("~p days, ",[X]).

ppsec2utc(Seconds) ->
    iso_8601_fmt(sec2utc(Seconds)).

get_db_pid() ->
    case whereis(dd_db) of
        undefined ->
            {dd_db,Pid,_,_} = hd(lists:filter(fun(X) -> is_db_process(X) end, supervisor:which_children(dd_sup))),
            register(dd_db, Pid),
            Pid;
        Pid -> Pid
    end.

is_db_process({dd_db,_,_,_}) -> true;
is_db_process(_) -> false.

localtime_ms() ->
    {_, _, Micro} = Now = os:timestamp(),
    {Date, {Hours, Minutes, Seconds}} = calendar:now_to_local_time(Now),
    {Date, {Hours, Minutes, Seconds, Micro div 1000 rem 1000}}.

format_time() ->
    format_time(localtime_ms()).

format_time({{Y, M, D}, {H, Mi, S, Ms}}) ->
    {io_lib:format("~b-~2..0b-~2..0b", [Y, M, D]),
     io_lib:format("~2..0b:~2..0b:~2..0b.~3..0b", [H, Mi, S, Ms])};
format_time({{Y, M, D}, {H, Mi, S}}) ->
    {io_lib:format("~b-~2..0b-~2..0b", [Y, M, D]),
     io_lib:format("~2..0b:~2..0b:~2..0b", [H, Mi, S])}.

iso_8601_fmt(DateTime) ->
    {{Year,Month,Day},{Hour,Min,Sec}} = DateTime,
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
        [Year, Month, Day, Hour, Min, Sec]).

%% @doc
%% When you have a proplist that contains {string(), list(any())} items, you can use this function
%% to add items to the list(any()).
%% This uses lists:append to make sure it gets appended to the end of the list.
%% Since we will use this to add modules to a specific channel configuration, I want that module to be run
%% after the original modules.
%% @end
add_item_to_proplist_value(Item, PropList, Key) ->
    case proplists:get_value(Key, PropList) of
        undefined -> PropList;
        Value ->
            [ {Key, lists:append(Value,[Item])}  |proplists:delete(Key, PropList)]
    end.

%% @doc
%% This does the reverse of the adding function.
%% @end 
remove_item_from_proplist_value(Item, PropList, Key) ->
    case proplists:get_value(Key, PropList) of
        undefined -> PropList;
        Value ->
            [ {Key, lists:delete(Item, Value)} | proplists:delete(Key, PropList)]
    end.


%% ================================
%% IRC PROTOCOL HELPERS
%% ================================

-spec request_version(ReplyPid :: pid(), Nick :: binary()) -> ok.
request_version(ReplyPid, Nick) ->
    %% needs to be some kind of caching here, so it only requests this once every 24 hours or so.
    dd_connection:send_msg(ReplyPid, <<>>, <<"CTCP">>, [Nick], <<"VERSION">>),
    ok.
-spec request_whois(ReplyPid :: pid(), Nick :: binary()) -> ok.
request_whois(ReplyPid, Nick) ->
    %% the response to this will be caught in the numeric reply handlers.
    dd_connection:send_msg(ReplyPid, <<>>, <<"WHOIS">>, [Nick], Nick),
    ok.
