%%% @author Jörgen Brandt <brandjoe@hu-berlin.de>
%%% @copyright 2016 Jörgen Brandt.
%%% @doc 

-module( localfs ).
-behaviour( fs_srv ).

-author( 'Jörgen Brandt <brandjoe@hu-berlin.de>' ).

-export( [init/1, stage_in/3, stage_out/3] ).

init( RepoDir ) when is_list( RepoDir ) ->
  RepoDir.

stage_in( RemoteFile, LocalDir, RepoDir )
when is_list( RemoteFile ),
     is_list( LocalDir ),
     is_list( RepoDir ) ->

  BaseName = filename:basename( RemoteFile ),
  LocalFile = lists:flatten( [LocalDir, $/, BaseName] ),
  Ld = lists:flatten( [LocalDir, $/] ),
  AbsRemoteFile = lists:flatten( [RepoDir, $/, RemoteFile] ),

  case filelib:ensure_dir( Ld ) of
    {error, R1} -> error( [{reason, R1}, {op, ensure_dir}, {arg, [Ld]}] );
    ok          ->
      case file:make_symlink( AbsRemoteFile, LocalFile ) of
        {error, R2} -> error( [{reason, R2},
                               {op, make_symlink},
                               {arg, [LocalFile, RemoteFile]}] );
        ok          -> ok
      end
  end.

stage_out( LocalFile, RemoteDir, RepoDir )
when is_list( LocalFile ),
     is_list( RemoteDir ),
     is_list( RepoDir ) ->

  BaseName = filename:basename( LocalFile ),
  RemoteFile = lists:flatten( [RepoDir, $/, BaseName] ),
  Rd = lists:flatten( [RepoDir, $/] ),
  case filelib:ensure_dir( Rd ) of
    {error, R1} -> error( [{reason, R1}, {op, ensure_dir}, {arg, [Rd]}] );
    ok          ->
      case file:make_symlink( LocalFile, RemoteFile ) of
        {error, R2} -> error( [{reason, R2},
                               {op, make_symlink},
                               {arg, [LocalFile, RemoteFile]}] );
        ok          -> ok
      end
  end.

