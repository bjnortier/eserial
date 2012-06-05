
-module(eserial_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, Options} = application:get_env(options),
    SerialServer = {eserial_server, {eserial_server, start_link, [Options]}, permanent, 5000, worker, [eserial_server]},
    {ok, { {one_for_one, 5, 10}, [SerialServer]} }.

