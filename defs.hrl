% This record defines the structure of the 
% client process. 
% 
% It contains the following fields: 
%
% gui: it stores the name (or Pid) of the GUI process.
%
-record(cl_st, {gui,nickname,server,chatrooms,status}).
    
% This record defines the structure of the 
% server process. 
% 
-record(server_st, {clients,chatrooms}).
