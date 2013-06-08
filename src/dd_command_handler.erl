%% Copyright 2012-2013 Gert Meulyzer

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
%%% Command interface to the bot.
%%% @end
%%% Created : 11 Nov 2012 by Gert Meulyzer <@G3rtm on Twitter>

-module(dd_command_handler).
-behaviour(dd_msghandler).
-include("../include/dd_irc.hrl").
-export([handle_msg/5]).

-export([reply_if_single_tweet/3, reply_if_single_tweet/1]).

-define(SingleTweetPattern, "https://twitter.com/(\\w*)/status/(\\d*)").

-spec handle_msg(pid(), binary(), binary(), [binary()], binary()) -> ok.
handle_msg(ReplyPid, _Prefix, <<"PRIVMSG">>, Args, <<"\\twit ", SearchString/binary>>) ->
    handle_twitter_search(ReplyPid, Args, SearchString);
handle_msg(ReplyPid, _Prefix, <<"PRIVMSG">>, Args, <<"\\@", Username/binary>>) ->
    handle_twitter_usertimeline(ReplyPid, Args, Username);
handle_msg(ReplyPid, _Prefix, <<"PRIVMSG">>, Args, <<"\\def ", SearchString/binary>>) ->
    handle_def_command(ReplyPid, Args, SearchString);
handle_msg(ReplyPid, _Prefix, <<"PRIVMSG">>, Args, <<"\\imdb -new ", SearchString/binary>>) ->
    handle_imdb_command(ReplyPid, Args, SearchString, new);
handle_msg(ReplyPid, _Prefix, <<"PRIVMSG">>, Args, <<"\\imdb -old ", SearchString/binary>>) ->
    handle_imdb_command(ReplyPid, Args, SearchString, old);
handle_msg(ReplyPid, _Prefix, <<"PRIVMSG">>, Args, <<"\\imdb ", SearchString/binary>>) ->
    handle_imdb_command(ReplyPid, Args, SearchString, new);
handle_msg(_ReplyPid, _Prefix, <<"PRIVMSG">>, Args, <<",bb 15">>) ->
    dd_connection:reply(self(), Args, <<"This command has been replaced by \\bb.">>);
handle_msg(ReplyPid, _Prefix, <<"PRIVMSG">>, Args, <<"\\bb ", BB/binary>>) ->
    handle_bb(ReplyPid, Args, BB);
handle_msg(ReplyPid, _Prefix, <<"PRIVMSG">>, Args, <<"\\numbertrivia ", Nr/binary>>) ->
    handle_numbertrivia(ReplyPid, Args, Nr);
handle_msg(ReplyPid, _Prefix, <<"PRIVMSG">>, Args, <<"\\numberinfo ", Nr/binary>>) ->
    handle_numberinfo(ReplyPid, Args, Nr);
handle_msg(ReplyPid, _Prefix, <<"PRIVMSG">>, Args, <<"\\translate ",Source:2/binary,"->",Target:2/binary," ",Phrase/binary>>) ->
    handle_translation(ReplyPid, Args, Source, Target, Phrase);
handle_msg(ReplyPid, _Prefix, <<"PRIVMSG">>, Args, <<"\\translate ",Source:2/binary,"|",Target:2/binary," ",Phrase/binary>>) ->
    handle_translation(ReplyPid, Args, Source, Target, Phrase);
handle_msg(_ReplyPid, _Prefix, _Command, _Args, _Tail) ->
    ok.

-spec handle_twitter_search(pid(), [binary()], binary()) -> ok.
handle_twitter_search(ReplyPid, Args, SearchString) ->
    URL = "http://search.twitter.com/search.json?rpp=5&q=" ++
        edoc_lib:escape_uri(binary_to_list(SearchString)),
    case dd_helpers:http_get(URL) of
        {error, _} -> ok;
        {ok, {_, _, JSON}} -> 
            TwtLst = get_tweets(JSON),
            [ reply_with_tweet(Tweet, ReplyPid, Args) || Tweet <- TwtLst ] 
    end.

-spec handle_def_command(pid(), [binary()], binary()) -> ok.
handle_def_command(ReplyPid, Args, SearchString) ->
    case dd_ddg:related_topics(SearchString, 2) of
        [] -> dd_connection:reply(ReplyPid, Args, dd_ddg:definition(SearchString));
        Lst -> [ dd_connection:reply(ReplyPid, Args, X) || X <- Lst ]
    end,
    ok.

-spec handle_imdb_command(pid(), [binary()], binary(), atom()) -> ok.
handle_imdb_command(P, Args, SearchString, old) ->
    case dd_imdb:oldest_movie(binary_to_list(SearchString)) of
        none -> ok;
        Info -> dd_connection:reply(P, Args, unicode:characters_to_binary(Info))
    end,
    ok;
handle_imdb_command(P, Args, SearchString, new) ->
    case dd_imdb:newest_movie(binary_to_list(SearchString)) of
        none -> ok;
        Info -> dd_connection:reply(P, Args, unicode:characters_to_binary(Info))
    end,
    ok;
handle_imdb_command(P, Args, SearchString, _) ->
    Info = dd_imdb:scrape(SearchString),
    dd_connection:reply(P, Args, unicode:characters_to_binary(Info)),
    ok.

-spec reply_with_tweet(string(), pid(), [binary()]) -> ok.
reply_with_tweet(Tweet, Pid, Args) ->
    spawn(fun() ->
                  dd_connection:reply(Pid, Args, unicode:characters_to_binary(Tweet))
          end),
    ok.

-spec get_tweets(string()) -> [string()].
get_tweets(JSON) ->
    P = mochijson:decode(JSON),
    {struct, PropList} = P,
    {array, TwtLst} = proplists:get_value("results", PropList),
    [ tweet_to_line(T) || T <- TwtLst ].

-spec tweet_to_line({'struct', [any()]}) -> string().
tweet_to_line({struct, P}) ->
    Nick = proplists:get_value("from_user", P),
    Name = proplists:get_value("from_user_name", P),
    Text = proplists:get_value("text", P),
    Name ++ " ("++Nick++"): "++Text.

-spec handle_twitter_usertimeline(pid(), [binary()], binary()) -> ok.
handle_twitter_usertimeline(ReplyPid, Args, Username) ->
    URL = "https://api.twitter.com/1/statuses/user_timeline.json?count=2&screen_name="++
        edoc_lib:escape_uri(binary_to_list(Username)),
    case dd_helpers:http_get(URL) of
        {error, _} -> ok;
        {ok, {_, _, JSON}} -> 
            {array, TwtList} = mochijson:decode(JSON),
            [ spawn(fun() -> reply_with_tweet(Tweet, ReplyPid, Args) end) 
              || Tweet <- [ get_usertimeline_tweet(Twt) || Twt <- TwtList ]]
    end,
    ok.
    
-spec get_usertimeline_tweet({'struct', [any()]}) -> string().
get_usertimeline_tweet({struct, Twt}) ->
    {struct, User} = proplists:get_value("user", Twt),
    Nick = proplists:get_value("screen_name", User),
    Name = proplists:get_value("name", User),
    Text = proplists:get_value("text", Twt),
    Name ++ " ("++Nick++"): "++Text.
    
-spec get_tweet_by_id(binary()) -> ok | string().
get_tweet_by_id(ID) ->
    URL = "https://api.twitter.com/1/statuses/show.json?id="++binary_to_list(ID),
    %% spawn(fun() ->
                  case dd_helpers:http_get(URL) of
                      {error, _} -> ok;
                      {ok, {_, _, JSON}} ->
                          TweetStruct = mochijson:decode(JSON),
                          get_usertimeline_tweet(TweetStruct)
                  end.
          %% end),
%%    ok.

-spec reply_if_single_tweet(binary(), [binary()], pid()) -> ok | false.
reply_if_single_tweet(URL, Args, Pid) ->
    {ok, Regex} = re:compile(?SingleTweetPattern, [caseless]),
    case re:run(URL, Regex, [{capture, all_but_first, binary}]) of
        {match, [_, TweetID]} -> 
            reply_with_tweet(get_tweet_by_id(TweetID), Pid, Args);
        _ -> false
    end.

-spec reply_if_single_tweet(binary()) -> ok | string().
reply_if_single_tweet(URL) ->
    {ok, Regex} = re:compile(?SingleTweetPattern, [caseless]),
    case re:run(URL, Regex, [{capture, all_but_first, binary}]) of
        {match, [_, TweetID]} -> 
            get_tweet_by_id(TweetID);
        _ -> false
    end.

-spec handle_bb(pid(), [binary()], binary()) -> ok.                       
handle_bb(P, Args, BB) ->
    Nr = list_to_integer(binary_to_list(BB)),
    case dd_bb:get_post_by_number(Nr) of
        none -> ok;
        R -> dd_connection:reply(P, Args, R)
    end,
    ok.

-spec handle_numbertrivia(pid(), [binary()], binary()) -> ok.                       
handle_numbertrivia(P, Args, Nr) ->
    dd_connection:reply(P, Args, ?U(dd_numbers:trivia(Nr))),
    ok.

-spec handle_numberinfo(pid(), [binary()], binary()) -> ok.                       
handle_numberinfo(P, Args, Nr) ->
    dd_connection:reply(P, Args, ?U(dd_numbers:math(Nr))),
    ok.

-spec handle_translation(pid(), [binary()], binary(), binary(), binary()) -> ok.
handle_translation(P, Args, Source, Target, Phrase) ->
    case dd_translation:translate(Phrase, Source, Target) of
        none -> ok;
        Reply -> dd_connection:reply(P, Args, Reply)
    end,
    ok.
