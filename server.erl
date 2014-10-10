-module(server).
-export([loop/2, initial_state/1]).

-include_lib("./defs.hrl").

 

loop(St, {connect,Client}) ->
    case check_nick(Client#cl_st.nickname,St#server_st.clients) of
		exist ->
			{cannot_connect,St};
		free ->
			{can_connect,St#server_st{clients=[Client#cl_st.nickname|St#server_st.clients]}}
	
	end;
	  
loop(St,{join,Client,Channel}) ->
	case channel_exist(Channel,St#server_st.chatrooms) of
			t ->
			  case client_exist(element(2,lists:keyfind(Channel,1,St#server_st.chatrooms)),Client#cl_st.gui) of
				 t ->
				  {cannot_join,St};
				 f ->
				  {can_join,
				  St#server_st{chatrooms=lists:keyreplace(Channel,1,St#server_st.chatrooms,{Channel,[Client#cl_st.gui|element(2,lists:keyfind(Channel,1,St#server_st.chatrooms))]})}}
			  end;
		    f ->
			    {can_join,
				   St#server_st{chatrooms=[{Channel,[Client#cl_st.gui]}|St#server_st.chatrooms]}}
	end;

		
loop(St,{message,Client,Channel,Msg}) -> 
	 case client_exist(element(2,lists:keyfind(Channel,1,St#server_st.chatrooms)),Client#cl_st.gui) of
			 
		t ->
		     spawn(fun() -> send_message(St,Msg,Channel,Client) end),
			{ok,St};
		f ->
			{error,St}
	end;


loop(St,{leave,Client,Channel}) ->
	 case client_exist(element(2,lists:keyfind(Channel,1,St#server_st.chatrooms)),Client#cl_st.gui) of
			
		t -> 
			{can_leave,
		 St#server_st{chatrooms=lists:keyreplace(Channel,1,St#server_st.chatrooms,{Channel,lists:delete(Client#cl_st.gui,element(2,lists:keyfind(Channel,1,St#server_st.chatrooms)))})}};

	    f ->
			{cannot_leave,St}
	end;

loop(St,{disconnect,Client}) ->
	{ok,St#server_st{clients=lists:delete(Client#cl_st.nickname,St#server_st.clients)}}.

send_message(St,Message,Channel,Client)->
	L = lists:delete(Client#cl_st.gui,get_chatters(Channel,St#server_st.chatrooms)),
	Send_message = fun(X) -> gen_server:call(list_to_atom(X), {msg_to_GUI, Channel, Client#cl_st.nickname++"> "++Message}) end,
	lists:foreach(Send_message,L).

	
get_chatters(_,[]) ->
		[];	
get_chatters(Channel,[Head|Rest]) ->
	 if
		element(1,Head) == Channel ->
			element(2,Head);
		true ->
			get_chatters(Channel,Rest)
    end.
 	
channel_exist(_Channel,[]) ->
	 f;
channel_exist(Channel,[Head|_]) when Channel==element(1,Head) ->
	 t;
channel_exist(Channel,[_|Rest]) ->
	channel_exist(Channel,Rest).

client_exist([],_) ->
	 f;
client_exist([Head|Rest],Gui) -> 
	if
	   Head == Gui -> 
		 t;
       true ->
	     client_exist(Rest,Gui)
		 
	end.
check_nick(_Name,[]) ->
	  free;
	  
check_nick(Name,[Head|Rest]) ->
	  if
		Name == Head ->
			exist;
		true ->
			check_nick(Name,Rest)
	  end.
		
initial_state(_Server) ->
    #server_st{clients=[],chatrooms=[]}.
