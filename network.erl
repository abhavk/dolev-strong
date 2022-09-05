-module(network).
-export([start_link/1]).
-export([init/1, handle_call/3]).
-behaviour(gen_server).
-include("message.hrl").

start_link(N) ->
    gen_server:start_link({local, network}, network, [N,0], []).

init([N, F]) ->
        Nodes = spawner(N-F,F,[]),
	io:fwrite("Network state initialized with Nodes ~w.~n", [Nodes]),
	NodeDict = dict:from_list(lists:map(fun(X)->{X,null} end, Nodes)),
	{ ok, { 0, NodeDict, []} }.

spawner(1, 0, Spawned) -> 
	Node = spwnsender(),
	lists:append(Spawned,[Node]);
spawner(0, 1, Spawned) -> 
	Node = spwn(false),
	lists:append(Spawned, [Node]);
spawner(N, 0, Spawned) ->
        Node = spwn(true),
        spawner(N-1,0, lists:append(Spawned,[Node]));
spawner(0, F, Spawned) ->
        Node = spwn(false),
        spawner(0, F-1,lists:append(Spawned,[Node]));
spawner(N, F,Spawned) ->
        Rand = rand:uniform(2)-1,
        if
                Rand == 0 ->
                        Node = spwn(true),
                        spawner(N-1,F, lists:append(Spawned, [Node]));
                Rand == 1 ->
                        Node = spwn(false),
                        spawner(N, F-1, lists:append(Spawned, [Node]));
                true ->
                        % do nothing
                        ok
        end.


spwn(X) ->
        if
                X -> 	
			Node = spawn(node, create, [0, false, 0, self()]),	
			io:fwrite("Spawned 1 honest node with Pid = ~w.~n", [Node]);
                true -> 
			Node = null,
			io:fwrite("Spawned 1 byzantine node.~n")
        end,
	Node.
	
spwnsender() ->
	Node = spawn(node, create, [0, true, 0, self()]),
	io:fwrite("Spawned 1 honest sender with Pid = ~w.~n", [Node]),
	Node.

handle_call({getAllNodes}, _From, State) -> 
	{ _Timestep, Nodes, _Outbox } = State,
	{ reply, dict:fetch_keys(Nodes), State };
handle_call({send, Signedtx}, From, State) -> 
	{ Timestep, Nodes, Outbox } = State,
	{ Pid, _Alias } = From,
	io:fwrite("Network received txn from ~w at timestep ~w. Pushing to outbox.~n", [Pid, Timestep]),
	{ reply, ok, { Timestep, Nodes, lists:append(Outbox, [Signedtx])} };
handle_call({step}, _From, State) -> 
	processObx(State),
	{ Timestep, Nodes, _Outbox } = State,
	broadcast_step(dict:fetch_keys(Nodes)),
	{ reply, ok, { Timestep + 1, Nodes, [] } };
handle_call({resolvepid, Pid}, _From, State) ->
	{ _Timestep, Nodes, _Outbox } = State,
	Pubkey = dict:fetch(Pid, Nodes),
	{ reply, Pubkey, State };	
handle_call({regn, Pubkey}, From, State) ->
	{ Timestep, Nodes, Outbox } = State,
	{ Pid, _Alias } = From,
	UpdatedDict = dict:store(Pid, Pubkey, Nodes),
	io:fwrite("Registering node ~w with public key ~w.~n", [Pid, Pubkey]),
	{ reply, ok, { Timestep, UpdatedDict, Outbox }};
handle_call({read}, _From, State) ->
	{ reply, State, State }.

processObx({ _Timestep, _Nodes, [] }) ->
        processed;
processObx({ Timestep, Nodes, [H|T]}) ->
	send(H, Timestep, Nodes),
	processObx({ Timestep, Nodes, T}).

send(M, _Timestep, Nodes) ->
        Pid = keyfromvalue(M#message.recipient, Nodes),
	Pid ! { msg, M }.

keyfromvalue(Value, Dict) -> 
	Dict2 = dict:filter(fun(_K,V) -> V==Value end, Dict),
	hd(dict:fetch_keys(Dict2)).

broadcast_step([]) ->
	broadcast_finished;
broadcast_step([H|T]) -> 
	H ! { step },
	broadcast_step(T).
