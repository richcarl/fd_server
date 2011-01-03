%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <greenmellon@gmail.com>
%%% @copyright (C) 2010 Fernando Benavides <greenmellon@gmail.com>
%%% @doc FD Server main application module
%%% @end
%%%-------------------------------------------------------------------
-module(fd_server_app).
-author('Fernando Benavides <greenmellon@gmail.com>').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
%% @hidden
-spec start(normal | {takeover, node()} | {failover, node()}, term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
  fd_server_sup:start_link().

%% @hidden
-spec stop([]) -> ok.
stop([]) -> ok.