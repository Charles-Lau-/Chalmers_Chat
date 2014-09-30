-module(server).
-export([loop/2, initial_state/1]).

-include_lib("./defs.hrl").

loop(St, {connect,Client}) ->
    case check_nick(Client#cl_st.nickname,St#server_st.clients) of
		exist ->
			{cannot_connect,St};
		free ->
			{can_connect,St#server_st{clients=[Client#cl_st{status=connected}|St#server_st.clients]}}
	end;
	  
loop(St,{join,Client,Channel}) ->
	case channel_exist(Channel,St#server_st.chatrooms) of
			t ->
			  case channel_exist(Channel,Client#cl_st.chatrooms) of
				 t ->
				  {cannot_join,St};
				 f ->
				  {can_join,
				   St#server_st{clients=update_clients(St#server_st.clients,Client#cl_st.gui,Client#cl_st{chatrooms=[Channel|Client#cl_st.chatrooms]}),chatrooms=[Channel|St#server_st.chatrooms]}}
			  end;
		    f ->
			    {can_join,
				   St#server_st{clients=update_clients(St#server_st.clients,Client#cl_st.gui,Client#cl_st{chatrooms=[Channel|Client#cl_st.chatrooms]}),chatrooms=[Channel|St#server_st.chatrooms]}}
	end;

loop(St,{message,Client,Channel,Msg}) ->
    
	case channel_exist(Channel,Client#cl_st.chatrooms) of 
		t ->
		     L = lists:keydelete(Client#cl_st.gui,2,get_chatters(Channel,St#server_st.clients,[])),
			 Send_message = fun(X) -> gen_server:call(list_to_atom(X#cl_st.gui), {msg_to_GUI, Channel, Client#cl_st.nickname++"> "++Msg}) end,
			 lists:foreach(Send_message,L),
			{ok,St};
		f ->
			{error,St}
	end;


loop(St,{leave,Client,Channel}) ->
	case channel_exist(Channel,Client#cl_st.chatrooms) of
		t ->
			{can_leave,St#server_st {clients=update_clients(St#server_st.clients,Client#cl_st.gui,Client#cl_st{chatrooms=lists:delete(Channel,Client#cl_st.chatrooms)})}};
	    f ->
			{cannot_leave,St}
	end;

loop(St,{disconnect,Client}) ->
	{ok,St#server_st{clients=lists:keydelete(Client#cl_st.gui,2,St#server_st.clients)}}.
	
update_clients(List,Guiname,New) ->
	  [New|lists:keydelete(Guiname,2,List)].
	  
get_chatters(_Channel,[],Result) ->
	     Result;	
get_chatters(Channel,[Head|Rest],Result) ->
	 case  channel_exist(Channel,Head#cl_st.chatrooms) of
	    t ->
			get_chatters(Channel,Rest,[Head|Result]);
		f ->
			get_chatters(Channel,Rest,Result)
	end.

 	
channel_exist(_Channel,[]) ->
	 f;
channel_exist(Channel,[Head|_]) when Channel==Head ->
	 t;
channel_exist(Channel,[_|Rest]) ->
	channel_exist(Channel,Rest).
	
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
