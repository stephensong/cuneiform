-module( hdfs ).
-behaviour( fs_srv ).

-export( [init/1, stage_in/3, stage_out/3] ).

-define( BUF_SIZE, 1024 ).

init( _Arg ) -> [].

stage_in( RemoteFile, LocalDir, _UserInfo )
when is_list( RemoteFile ),
     is_list( LocalDir ) ->

  Cmd = lists:flatten( string:join(
          ["hadoop", "fs", "-copyToLocal", "-f", RemoteFile, LocalDir], " " ) ),



  Port = open_port( {spawn, Cmd}, [exit_status,
                     stderr_to_stdout,
                     binary,
                     {line, ?BUF_SIZE}] ),

  listen_port( Port, [], <<>>, stage_in, RemoteFile ).


stage_out( LocalFile, RemoteDir, _UserInfo )
when is_list( LocalFile ),
     is_list( RemoteDir ) ->

  Cmd = lists:flatten( string:join(
          ["hadoop", "fs", "-copyFromLocal", "-f", LocalFile, RemoteDir], " " ) ),



  Port = open_port( {spawn, Cmd}, [exit_status,
                     stderr_to_stdout,
                     binary,
                     {line, ?BUF_SIZE}] ),

  listen_port( Port, [], <<>>, stage_out, LocalFile ).



listen_port( Port, OutAcc, LineAcc, Op, File )
when is_port( Port ),
     is_list( OutAcc ),
     is_binary( LineAcc ),
     is_atom( Op ),
     is_list( File ) ->

  receive

    {Port, {data, {noeol, PartLine}}} ->
      listen_port( Port, OutAcc, <<LineAcc/binary, PartLine/binary>>, Op, File );

    {Port, {data, {eol, PartLine}}} ->
      Line = <<LineAcc/binary, PartLine/binary>>,
      listen_port( Port, [Line|OutAcc], <<>>, Op, File );

    {Port, {exit_status, 0}} ->
      ok;

    {Port, {exit_status, _}} ->
      error( [{reason, port_failed},
              {op, Op},
              {file, File},
              {out, lists:reverse( OutAcc )}] );

    Msg ->

      error_logger:error_report( [{reason, invalid_msg},
                                  {msg, Msg},
                                  {action, ignored}] ),

      listen_port( Port, OutAcc, LineAcc, Op, File )

  end.