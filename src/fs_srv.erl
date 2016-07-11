%%% @author Jörgen Brandt <brandjoe@hu-berlin.de>
%%% @copyright 2016 Jörgen Brandt.
%%% @doc 

-module( fs_srv ).
-behaviour( gen_server ).
-define( SERVER, ?MODULE ).

-author( 'Jörgen Brandt <brandjoe@hu-berlin.de>' ).

%%====================================================================
%% Function Exports
%%====================================================================

-export( [start_link/2, stage_in/2, stage_out/2] ).

-export( [init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2,
         code_change/3] ).

%%====================================================================
%% User Module Callback Functions
%%====================================================================

-callback init( UserArg::_ ) -> _.

-callback stage_in( RemoteFile, LocalDir, UserInfo ) -> ok
when RemoteFile :: string(),
     LocalDir   :: string(),
     UserInfo   :: _.

-callback stage_out( LocalFile, RemoteDir, UserInfo ) -> ok
when LocalFile :: string(),
     RemoteDir :: string(),
     UserInfo  :: _.

%%====================================================================
%% Internal Record Definitions
%%====================================================================

-record( mod_state, { mod, user_info } ).

%%====================================================================
%% API Function Definitions
%%====================================================================

-spec start_link( Mod, UserArg ) -> {ok, Pid}
when Mod     :: atom(),
     UserArg :: _,
     Pid     :: pid().

start_link( Mod, UserArg ) when is_atom( Mod ) ->
  gen_server:start_link( {local, ?SERVER}, ?MODULE, [Mod, UserArg], [] ).


-spec stage_in( RemoteFile, LocalDir ) -> Pid
when RemoteFile :: string(),
     LocalDir   :: string(),
     Pid        :: pid().

stage_in( RemoteFile, LocalDir ) ->
  gen_server:call( ?SERVER, {stage_in, RemoteFile, LocalDir} ).

-spec stage_out( LocalFile, RemoteDir ) -> Pid
when LocalFile :: string(),
     RemoteDir :: string(),
     Pid       :: pid().

stage_out( LocalFile, RemoteDir ) ->
  gen_server:call( ?SERVER, {stage_out, LocalFile, RemoteDir} ).

%%====================================================================
%% gen_server Function Definitions
%%====================================================================

init( [Mod, UserArg] ) ->

  UserInfo = Mod:init( UserArg ),

  {ok, #mod_state{ mod = Mod, user_info = UserInfo }}.

handle_call( {stage_in, RemoteFile, LocalDir}, _From,
             State = #mod_state{ mod = Mod, user_info = UserInfo } )

when is_list( RemoteFile ),
     is_list( LocalDir ),
     is_atom( Mod ) ->

  Pid = spawn( fun() ->
                 Mod:stage_in( RemoteFile, LocalDir, UserInfo )
               end ),

  {reply, Pid, State};

handle_call( {stage_out, LocalFile, RemoteDir}, _From,
             State = #mod_state{ mod = Mod, user_info = UserInfo } )

when is_list( LocalFile ),
     is_list( RemoteDir ),
     is_atom( Mod ) ->

  Pid = spawn( fun() ->
                 Mod:stage_out( LocalFile, RemoteDir, UserInfo )
               end ),

  {reply, Pid, State}.

handle_cast( _Msg, State ) ->
  {noreply, State}.

handle_info( _Info, State ) ->
  {noreply, State}.

terminate( _Reason, _State ) ->
  ok.

code_change( _OldVsn, State, _Extra ) ->
  {ok, State}.

%%====================================================================
%% Internal Function Definitions
%%====================================================================
