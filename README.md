# Dolev-Strong

This is an implementation of the dolev-strong protocol, as outlined in lecture 2 of the series [Foundations of Blockchains](https://www.youtube.com/watch?v=T59hTifuwGU&list=PLEGCF-WLh2RLOHv_xUGLqRts_9JxrckiA&index=6&ab_channel=TimRoughgardenLectures). 

## Byzantine Broadcast (Problem)

Dolev-Strong is a simple protocol for Byzantine Broadcast, the problem of a single node (known as the sender) broadcasting a value and having all nodes that follow this protocol agree on the same value. 

Precisely, a protocol solving Byzantine Broadcast has the following properties: 
- Termination: Each honest node eventually outputs some value. 
- Agreement: All honest nodes output the same value. 
- Validity: If sender is honest, all honest nodes output the value that the sender broadcasts. 

## The Protocol
The protocol involves all honest nodes comparing notes among each other for the values sent by the sender. The goal is to catch a byzantine sender red-handed, by comparing notes that have come from the sender. If the sender is caught sending multiple values, then nodes know to ignore this sender and output `⊥` - representing a null output. 

Note that the case for an honest sender is almost trivial. It will only ever broadcast a single value, and that value can be verified as having come from the sender by all honest nodes. 

The protocol detail is explained in comments in the file `node.erl`. 

## Assumptions 

The dolev-strong protocol is applicable under the assumptions: 
- PKI: Public Key infrastructure is in place before the protocol starts
- Permissioned: IP of all nodes, and in particular that of the sender, is known when the protocol starts (although sender might be byzantine)
- Byzantine: Number of faulty nodes is known (`f`/`LittleF`) and `f < N`
- Synchronicity: Synchronous network. All messages arrive within the next timestep. No guarantee on order of delivery within a timestep. 

## Implementation
This implementation of Dolev-Strong is written in [Erlang](https://www.youtube.com/watch?v=BXmOlCy0oBM&ab_channel=CH1LLW4VE). Why Erlang? Erlang is a simple functional process-based language. What are distributed networks if not simple message-passing systems? Also the default [Arweave node](https://github.com/ArweaveTeam/arweave/tree/master/apps/arweave/src) is written in Erlang. 

The implementation outlines 2 basic behaviours, one for the network, and one for the node. 

The reason the network is separated into its own server is to provide an abstraction for time and message delivery delays. The same abstraction can be used in future by the adversary in asynchronous or partially synchronous models. 

The network is responsible for message passing, keeping track of timestep, and keeping a registry of Pid to Publickey mappings. 

### Sample run

In order to run the network, you must have erlang installed. 

Then, 
```
$ erl 
1> c(node).
{ok,node}
2> c(network).
network.erl:4:2: Warning: undefined callback function handle_cast/2 (behaviour 'gen_server')
%    4| -behaviour(gen_server).
%     |  ^

{ok,network}
```

Start the network, 
```
3> network:start_link(2).
Spawned 1 honest node with Pid = <0.93.0>.
Input is 0 and Sender == false. 
Spawned 1 honest sender with Pid = <0.94.0>.
Input is 0 and Sender == true. 
Network state initialized with Nodes [<0.93.0>,<0.94.0>].
{ok,<0.92.0>}
Registering node <0.94.0> with public key <<77,20,41,117,32,165,127,165,35,209,88,187,109,218,112,31,203,71,26,33,210,151,43,213,31,234,12,243,18,38,235,142>>.
Registering node <0.93.0> with public key <<147,232,96,185,88,31,205,204,107,137,127,167,158,203,114,83,178,12,17,202,39,88,18,173,7,22,222,152,190,125,18,57>>. 
Node <0.94.0> broadcasting 0 to the network.
Node <0.93.0> listening as ( Sender? = false )
Signing message from sender = <0.94.0> (alias) and content = {0,[]}.
Network received txn from <0.94.0> at timestep 0. Pushing to outbox.
Node <0.94.0> listening as ( Sender? = true )
```
The above code spawned 2 processes - an honest sender and an honest non-sender. It also "registered" the processes with network (this is only an implementation detail and the network is technically p2p in the Byzantine Broadcast. See the Implementation section above for explanation) 

Move to the next timestep, 
```
4> gen_server:call(network, { step }).
Process <0.93.0> received message {message,<<77,20,41,117,32,165,127,165,35,209,88,187,109,218,112,31,203,71,26,33,210,151,43,213,31,234,12,243,18,38,235,142>>,<<147,232,96,185,88,31,205,204,107,137,127,167,158,203,114,83,178,12,17,202,39,88,18,173,7,22,222,152,190,125,18,57>>,{0,[]},<<97,46,60,181,78,3,62,83,145,146,185,151,223,108,206,7,237,197,115,117,180,239,231,45,77,177,221,166,79,140,246,49,181,110,8,26,24,181,204,214,251,18,139,99,90,154,205,223,177,42,204,239,39,26,133,168,98,51,233,48,96,173,142,8>>}. 


Node <0.94.0> convinced of values [0].
ok
Signing message from sender = <0.93.0> (alias) and content = {0,[<<77,20,41,117,32,165,127,165,35,209,88,187,109,218,112,31,203,71,26,33,210,151,43,213,31,234,12,243,18,38,235,142>>]}.
Network received txn from <0.93.0> at timestep 1. Pushing to outbox.
Node <0.93.0> listening as ( Sender? = false )
   
Node <0.93.0> convinced of values [0].
```

The code above logged a message received by the honest non-sender (`<0.93.0>`). Since there are no Byzantine nodes in this run (`f = 0`), output is at time `t=1`. This is represented by the two lines, 

```
Node <0.94.0> convinced of values [0]. 
Node <0.93.0> convinced of values [0].
```

## Future Work
This is a simplistic implementation that captures the main idea behind dolev-strong. Possible extensions and future work:
- Spot the bug (byzantine nodes can currently just lie about a crucial detail in this implementation - it has something to do with signature verification)
- Implement a Byzantine node
- Make the network robust to all attacks and faulty input
- Extend to the FLP impossibility result for asynchronous models
- Extend to other protocol implementations (tendermint, etc.)
