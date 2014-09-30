-module(client).
-export([loop/2, initial_state/2]).

-include_lib("./defs.hrl").

%%%%%%%%%%%%%%%
%%%% Connect
%%%%%%%%%%%%%%%
loop(St, {connect, Server}) ->
	Ref =  make_ref(), 
    list_to_atom(Server) ! {request,self(),Ref,{connect,St}},
	receive
		{result,_,can_connect} ->
			 
			{ok,St#cl_st{status=connected,server=list_to_atom(Server)}} ;
		{result,_,cannot_connect} ->
			{{error,user_already_connected,"user_already_connected"},St} 
		  
	end;

%%%%%%%%%%%%%%%
%%%% Disconnect
%%%%%%%%%%%%%%%
loop(St, disconnect) ->
    if
		St#cl_st.status == unconnected ->
			{{error,user_not_connected,atom_to_list(user_not_connected)},St};
		St#cl_st.chatrooms /= [] ->
     		{{error,leave_channels_first,atom_to_list(leave_channels_first)},St};
        true ->
			Ref = make_ref(),
			St#cl_st.server ! {request,self(),Ref,{disconnect,St}},
			receive	
				{result,_,ok} ->
					{ok,St#cl_st{status=nonconnected}}
			end
	end;

%%%%%%%%%%%%%%
%%% Join
%%%%%%%%%%%%%%
loop(St,{join,Channel}) ->
	Ref = make_ref(),
	St#cl_st.server ! {request,self(),Ref,{join,St,Channel}},
	receive
		{result,_,cannot_join} ->
			{{error,user_already_joined,"user_already_joined"},St};
		{result,_,can_join} ->
			{ok,St#cl_st{chatrooms=[Channel|St#cl_st.chatrooms]}}
			
    end;

%%%%%%%%%%%%%%%
%%%% Leave
%%%%%%%%%%%%%%%
loop(St, {leave, Channel}) ->
	  Ref = make_ref(),
	  St#cl_st.server ! {request,self(),Ref,{leave,St,Channel}},
	  receive
		 {result,_,cannot_leave} ->
			{{error,user_not_joined,"user_not_joined"},St};
		 {result,_,can_leave} ->
			{ok,St#cl_st{chatrooms=lists:delete(Channel,St#cl_st.chatrooms)}}
     end;

%%%%%%%%%%%%%%%%%%%%%
%%% Sending messages
%%%%%%%%%%%%%%%%%%%%%
loop(St, {msg_from_GUI, Channel, Msg}) ->
	 Ref = make_ref(),
	 St#cl_st.server ! {request,self(),Ref,{message,St,Channel,Msg}},
	 receive
		{result,_,error} ->
			 {{error,user_not_joined,"user_not_joined"},St};
		{result,_,ok} -> 
			 {ok,St}
     end;


%%%%%%%%%%%%%%
%%% WhoIam
%%%%%%%%%%%%%%
loop(St, whoiam) ->
    {St#cl_st.nickname, St} ;

%%%%%%%%%%
%%% Nick
%%%%%%%%%%
loop(St,{nick,Nick}) ->
	{ok, St#cl_st{nickname = Nick}} ;

%%%%%%%%%%%%%
%%% Debug
%%%%%%%%%%%%%
loop(St, debug) ->
    {St, St} ;

%%%%%%%%%%%%%%%%%%%%%
%%%% Incoming message
%%%%%%%%%%%%%%%%%%%%%
loop(St = #cl_st { gui = GUIName }, MsgFromClient) ->
    {Channel, Name, Msg} = decompose_msg(MsgFromClient),
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {ok, St}.


% This function will take a message from the client and
% decomposed in the parts needed to tell the GUI to display
% it in the right chat room.
decompose_msg([Head|Rest]) ->
	[N|Restt] = Rest,
	[M|_] = Restt,
	{Head,N, M}.


initial_state(Nick, GUIName) ->
    #cl_st { gui = GUIName,
		  nickname = Nick,
		  chatrooms = [],
		  server = none,
		  status = unconnected}.
