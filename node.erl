% This is a node that implements the Dolev-Strong protocol for Byzantine Broadcast
% Byzantine broadcast is a single-shot consensus problem, that has one sender. The protocol guarantees termination, liveness and consistency in the synchronous model with public key infrastructure.

%% Node behaviour (honest nodes)
%% If sender
% 	- Sends private input with signature to all nodes
% 	- If receives new message that it is convinced of, forwards to all node
%		-- Convinced of a message if it has t independent signatures by time step t AND the first signature is that of the sender
%% If non-sender
%	- If receives new message that it is convinced of, forwards to all node
%		-- Convinced of a message if it has t independent signatures by time step t
%  If at time step f+1 && convinced of exactly 1 value, output that value
%  If convinced of more than 1 value output 0
-module(node).
-export([create/4]).
-record(message, {
	sender,
	recipient,
	% content = { Value, [OtherSenders]}
	content, 
	signature
}).
-define(SIG_ALG, eddsa).
-define(DIGEST_ALG, sha256).
-define(TYPE, ed25519).

create(F, IsSender, Input, Network) -> 
	io:fwrite("Input is ~w and Sender == ~w. ~n", [Input, IsSender]),
	{ Pubkey, Privkey } = new_key(),
	gen_server:call(Network, { regn, Pubkey }),
	step(0, F, IsSender, Input, Pubkey, Privkey, [], Network).

listen(Time, LittleF, IsSender, Input, Pubkey, Privkey, Convincedof, Network) -> 
	io:fwrite("Node ~w listening as ( Sender? = ~w )~n", [self(), IsSender]),
	receive 
		{ msg, Message } -> 
			io:fwrite("Process ~w received message ~w. ~n~n", [self(), Message]),
			{ Value, Senders } = Message#message.content,
			NewlyConvinced = newlyConvincedOf(Value, Message, Time, Convincedof),
			if 
				NewlyConvinced -> 
					Allnodes = gen_server:call( Network, { getAllNodes }),
					Content = { Value, lists:append([Message#message.sender], Senders) },
					broadcast(allbutme(Allnodes), Privkey, Pubkey, Content, Network),
					listen(Time, LittleF, IsSender, Input, Pubkey, Privkey, lists:append([Value], Convincedof), Network);
				true -> 
					listen(Time, LittleF, IsSender, Input, Pubkey, Privkey, Convincedof, Network)
			end;
		{ step } -> 
			step(Time+1, LittleF, IsSender, Input, Pubkey, Privkey, Convincedof, Network)
	end.			

step(Time, LittleF, IsSender, Input, Pubkey, Privkey, Convincedof, Network) -> 
	if
		(Time == 0) ->
			if 
			 	IsSender -> 
					Allnodes = gen_server:call( Network, { getAllNodes }),
					io:fwrite("Node ~w broadcasting ~w to the network.~n", [self(), Input]),
					broadcast(allbutme(Allnodes), Privkey, Pubkey, { Input, [] }, Network),
					listen(Time+1, LittleF, IsSender, Input, Pubkey, Privkey, [Input], Network);
				true -> 
					% proceed with other stuff
                        		listen(Time+1, LittleF, IsSender, Input, Pubkey, Privkey, Convincedof, Network)
			end;
		(Time > LittleF) -> 
			% End protocol - resolve and output something
			output(Convincedof);
		true -> 
			% proceed with other stuff
			listen(Time+1, LittleF, IsSender, Input, Pubkey, Privkey, Convincedof, Network)
	end.

sign(Privkey, Message) -> 
	io:fwrite("Signing message from sender = ~w (alias) and content = ~w.~n", [self(), Message#message.content]),
	crypto:sign(?SIG_ALG, ?DIGEST_ALG, tx_data(Message), [ Privkey, ?TYPE ]).

new_key() -> crypto:generate_key(?SIG_ALG, ?TYPE).

broadcast([], _Privkey, _Pubkey, _Content, _Network) ->
	done;
broadcast(Nodes, Privkey, Pubkey, Content, Network) -> 
	[ H | T ] = Nodes,
	send(H, Privkey, Pubkey, Content, Network),
	broadcast(T, Privkey, Pubkey, Content, Network).

tx_data(#message{ sender = Sender, recipient = Recipient, content = Content }) -> 
	B2 = term_to_binary(Content),
	<< Sender/binary, Recipient/binary, B2/binary >>.

send(RecipientPid, Privkey, Pubkey, Content, Network) -> 
	Recipient = gen_server:call(Network, {resolvepid, RecipientPid}),
	Unsigned = #message{ 
		sender = Pubkey,
		recipient = Recipient,
		content = Content
	},
	Signed = Unsigned#message { signature = sign(Privkey, Unsigned) },
	% send to coordinator
	gen_server:call(Network, { send, Signed }).

newlyConvincedOf(Value, Message, Timestep, Convincedof) -> 
	AlreadyConvinced = valueIn(Value, Convincedof),
	if 
		AlreadyConvinced -> false;
		true -> 
			{ _V, Senders } = Message#message.content,		
			Unique = countUniqueSenders(Senders, [Message#message.sender], 1),
			if
				(Unique >= Timestep) -> true;
				true -> false
			end
	end.			

countUniqueSenders([], _List, N) ->
	N;
countUniqueSenders([H|T], List, N) -> 
	Duplicate = valueIn(H, List),
	if 
		Duplicate -> countUniqueSenders(T, List, N);
		true -> countUniqueSenders(T, [ H | List ], N+1)
	end.

valueIn(_Value, []) ->
	false;
valueIn(Value, [ H | T ]) ->
	if 
		(Value == H) -> 
			true;
		true -> 
			valueIn(Value, T)
	end.

allbutme(Nodes) -> 
	lists:filter(fun(X) -> isnotme(X) end, Nodes).

isnotme(X) -> 
	if 
		(X == self()) -> false;
		true -> true
	end.
			

output(Values) -> io:fwrite("~nNode ~w convinced of values ~w.~n", [self(), Values]).
