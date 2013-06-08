%%% @copyright (C) 2013, Gert Meulyzer
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
%%% Monitors and logs the activity of nicks.
%%% @end
%%% Created : 11 Mar 2013 by Gert Meulyzer <@G3rtm on Twitter>

-module(dd_activity).

-include("../include/dd_irc.hrl").

-export([handle_msg/5]).

-spec handle_msg(pid(), binary(), binary(), [binary()], binary()) -> ok.
handle_msg(ReplyPid, Prefix, <<"PRIVMSG">>, Args, <<"\\seen ", SearchString/binary>>=Msg) ->
    [Channel] = Args,
    last_seen(ReplyPid, Args, SearchString),
    store_activity(dd_db, Prefix, Channel, Msg);
handle_msg(_ReplyPid, Prefix, <<"PRIVMSG">>, [Channel] , Msg) ->
    store_activity(dd_db, Prefix, Channel, Msg);
handle_msg(_, _, _, _, _) -> ok.

store_activity(Pid, Prefix, Channel, Msg) ->
    dd_sql:store_activity(Pid, dd_ircmsg:nick_from_prefix(Prefix), Channel, Msg).

last_seen(Pid, Args, SearchString) ->
    case dd_sql:last_activity_for_nick(dd_db, SearchString) of
        [{columns,["nick", "tstamp", "channel", "msg"]}, {rows, [{Nick, Stamp, Channel, Msg}]}] ->
            Duration = dd_helpers:pptimediff(dd_helpers:diff2now(Stamp)),
            dd_connection:reply(Pid, Args,
                                iolist_to_binary([Nick, <<" was last seen on ">>, Channel,<<" ">>,Duration, <<"ago saying: ">>,Msg]));
        _ -> dd_connection:reply(Pid, Args, iolist_to_binary([<<"No record of ">>, SearchString]))
    end.
