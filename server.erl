-module(server).
-export([loop/2, initial_state/1,check_nick/2]).

-include_lib("./defs.hrl").

loop(St, {connect,Client}) ->
    case check_nick(Client#cl_st.nickname,St#server_st.clients) of
		exist ->
			{error,St};
		free ->
			{ok,St#server_st{clients=[Client|St#server_st.clients]}}
	end.
	  
	 
	
check_nick(_Name,[]) ->
	  free;
	  
check_nick(Name,[Head|Rest]) ->
	  if
		Name == Head#cl_st.nickname ->
			exist;
		true ->
			check_nick(Name,Rest)
	  end.
		
initial_state(_Server) ->
    #server_st{clients=[],chatrooms=[]}.
