-module(eserial_server).
-author('jb@erix.ericsson.se').
-author('tobbe@cslab.ericsson.se').
-author('bjnortier@gmail.com').
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1, stop/0, speed/1, connect/0, disconnect/0,
         open/1, close/0, send/1, dequeue/0, parity_odd/0, parity_even/0, break/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              Public API                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).


stop() ->
    gen_server:call(?MODULE, stop).

connect() ->
    gen_server:cast(?MODULE, connect).

disconnect() ->
    gen_server:cast(?MODULE, disconnect).

speed(Speed) when is_integer(Speed) ->
    gen_server:cast(?MODULE, {speed, Speed}).

open(TTY) ->
    gen_server:cast(?MODULE, {open, TTY}).

close() ->
    gen_server:cast(?MODULE, close).

send(Bytes) ->
    gen_server:cast(?MODULE, {send, Bytes}).

dequeue() ->
    gen_server:call(?MODULE, dequeue).

parity_odd() ->
    gen_server:cast(?MODULE, {parity, odd}).

parity_even() ->
    gen_server:cast(?MODULE, {parity, even}).

break() ->
    gen_server:cast(?MODULE, break).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              gen_server                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

-define(SEND,0).
-define(CONNECT,1).
-define(DISCONNECT,2).
-define(OPEN,3).
-define(CLOSE,4).
-define(SPEED,5).
-define(PARITY_ODD,6).
-define(PARITY_EVEN,7).
-define(BREAK,8).

-record(state, {port, received, logger}).

-define(INFO(Msg, Args), log(State, info, Msg, Args)).
-define(WARN(Msg, Args), log(State, warning, Msg, Args)).
-define(ERR(Msg, Args), log(State, error, Msg, Args)).

init(Options) ->
    Logger = proplists:get_value(logger, Options, eserial_io_logger),
    TTYArg = case proplists:get_value(tty, Options) of
                 undefined -> "";
                 TTY -> " -tty " ++ TTY
             end,
    SpeedArg = case proplists:get_value(speed, Options) of
                   undefined -> "";
                   Speed -> " -speed " ++ integer_to_list(Speed)
               end,
    process_flag(trap_exit,true),
    Logger:info("Starting with args: ~s ~s", [TTYArg, SpeedArg]),
    Port = open_port({spawn, priv_dir() ++ "/eserial -erlang" ++ TTYArg ++ SpeedArg},
                     [binary,
                      {packet,2}]),
    {ok, #state{port = Port, received = queue:new(), logger = Logger}}.

handle_call(dequeue, _From, State = #state{ received = Received }) ->
    case queue:out(Received) of
        {{value, Value}, Received1} ->
            {reply, {value, Value}, State#state{ received = Received1 }};
        {empty, _} ->
            {reply, empty, State}
    end;
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(Msg, _From, State) ->
    io:format("Unknown call to ~p: ~p~n", [?MODULE, Msg]),
    {reply, ok, State}.

handle_cast({send, Bytes}, State) ->
    send_serial(State, [?SEND, Bytes]),
    {noreply, State};
handle_cast({open, TTY}, State) ->
    ?INFO("opening ~p~n", [TTY]),
    send_serial(State, [?OPEN,TTY]),
    {noreply, State};
handle_cast(close, State) ->
    send_serial(State, [?CLOSE]),
    {noreply, State};
handle_cast(connect, State) ->
    send_serial(State, [?CONNECT]),
    {noreply, State};
handle_cast(disconnect, State) ->
    send_serial(State, [?DISCONNECT]),
    {noreply, State};
handle_cast({speed, SpeedIn, SpeedOut}, State) ->
    send_serial(State,[?SPEED, integer_to_list(SpeedIn)," ",integer_to_list(SpeedOut),0]),
    {noreply, State};
handle_cast(parity_odd, State) ->
    send_serial(State, [?PARITY_ODD]),
    {noreply, State};
handle_cast(parity_even, State) ->
    send_serial(State, [?PARITY_EVEN]),
    {noreply, State};
handle_cast(break, State) ->
    send_serial(State, [?BREAK]),
    {noreply, State};
handle_cast(Msg, State) ->
    ?INFO("Unknown cast to ~p: ~p~n", [?MODULE, Msg]),
    {noreply, State}.

handle_info({Port, {data, Bytes}}, State = #state{ received = Received,
                                                   port = Port }) ->
    Received1 = lists:foldl(fun(Byte, Q) ->
                                    queue:in(Byte, Q)
                            end,
                            Received,
                            binary_to_list(Bytes)),
    {noreply, State#state{ received = Received1 }};
handle_info({'EXIT', Port, Reason}, State) when Port =:= State#state.port->
    ?WARN("~p port has exited: ~p~n", [?MODULE, Reason]),
    {stop, port_process_terminated, State};
handle_info(Info, State) ->
    ?WARN("~p unknown info: ~p~n", [?MODULE, Info]),
    {noreply, State}.

terminate(Reason, State) ->
    ?INFO("terminating ~p: ~p~n", [?MODULE, Reason]),
    Port = State#state.port,
    Port ! {self(), close},
    receive 
        {Port, closed} ->
            ok;
        Err ->
            throw(Err)
    after 5000 ->
            throw(no_close_received)
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                private                                   %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

log(#state{logger = undefined}, _Level, _Msg, _Args) ->
    ok;
log(#state{logger = Logger}, Level, Msg, Args) ->
    Logger:Level(Msg, Args).
      
send_serial(#state{port = Port}, Message) ->
    Port ! {self(), {command, Message}}.


priv_dir() ->
    {file,ObjFile} = code:is_loaded(?MODULE),
    Dir = filename:dirname(ObjFile),
    Dir2 = filename:dirname(Dir),
    filename:join(Dir2, priv).
