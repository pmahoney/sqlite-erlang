%%%-------------------------------------------------------------------
%%% File    : test.erl
%%% Author  : manywaypark <manywaypark@gmail.com>
%%% Description : some test codes.
%%%
%%% Created : 11 Aug 2008 by manywaypark <manywaypark@gmail.com>
%%%-------------------------------------------------------------------
-module(test).

-compile(export_all).

-define(ALL_MODS, [sqlite, sqlite_lib]).

%% does unit test (on all modules w/o tty output)
unit() ->
    unit(false,?ALL_MODS). % no tty output by default for your eye(s).

%% does unit tests on modules
-spec(unit/2 :: (TTY :: boolean(), Mods :: [atom()]) -> 'done').
unit(TTY, Mods) ->
    error_logger:tty(TTY),
    lists:foreach(fun(M) ->
			  io:format("testing:~p~n", [M]),
			  M:test() end, Mods),
    done.

%% does coverage test (unit test only for now)
cover() ->
    cover(test, unit, ?ALL_MODS).

%% does coverage test (for Mods by invoking M:F)
-spec(cover/3 :: (M :: atom(), F :: atom(), Mods :: [atom()]) -> 'done').
cover(M,F,Mods) ->
    lists:foreach(fun(Mod) ->
			  io:format("re-compiling for coverage test:~p~n", [Mod]),
			  cover:compile(Mod, [{d, 'INCLUDE_TEST'}]) end, Mods),
    cover:start(),
    M:F(),
    lists:foreach(fun(Mod) ->
			  io:format("writing coverage test result:~p~n", [Mod]),
			  cover:analyse_to_file(Mod) end, Mods),
    cover:stop(),
    io:format("now, check your ~p.COVER.out file(s)!!~n", [Mods]),
    done.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include_lib("eunit/include/eunit.hrl").
