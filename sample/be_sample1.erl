%% -*- mode: erlang; erlang-indent-level: 4; indent-tabs-mode: nil -*-
%% -------------------------------------------------------------------
%%
%% Copyright (c) 2016 Basho Technologies, Inc.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
%%
%% Sample of a riak_backend implementation.
%%
%% Note that we prefer to throw a badarg exception, with arguments, over
%% dumping a function_clause exception on the user - it's just good manners.
%%
%%
-module(be_sample1).
-behavior(riak_backend).

-compile(no_auto_import).

-export([
    implements/0,
    implements/1,
    init/1,
    start/2,
    stop/1,
    supports/0,
    supports/1
]).

-export_type([
    bucket/0,
    key/0
]).

%%
%% Exported overrides of riak_backend bucket and key types with tighter
%% constraints.  These match Riak.  No need to override value(), as it
%% stays the same.
%%

-type bucket()      :: binary() | {binary(), binary()}.
%% A backend storage bucket.

-type key()         :: term().
%% A backend record key.

%% API version.
-define(API_VSN,    1).
%% Basic CRUD datastore.
-define(SUPPORTS,   ['create', 'read', 'update', 'delete']).

%%
%% Define the handle types.  These are opaque, so they shouldn't be exported.
%%

-record(impl, {
    a_version   = ?API_VSN  :: riak_backend:api_version(),
    b_version               :: riak_backend:be_version(),
    flags       = ?SUPPORTS :: riak_backend:features(),
    handle                  :: term(),
    lib                     :: binary()
}).

-record(inst, {
    handle          :: term(),
    data_path       :: binary(),
    flags           :: [atom()],
    impl            :: #impl{}
}).

-spec implements() -> riak_backend:interfaces().
implements() ->
    ['riak_backend'].


-spec implements(riak_backend:interface())
        -> riak_backend:api_version() | 'false'.
implements('riak_backend') ->
    ?API_VSN;
implements(_) ->
    'false'.


-spec init(riak_backend:config()) -> #impl{} | riak_backend:error().
init(_Config) ->
    %
    % Maybe load a NIF library or some such ...
    % keep info obtained from loading as needed
    %
    LIB = <<"path-to-NIF-library">>,    
    HND = <<"probably-a-reference-to-internal-state">>,
    BEV = "0.1.0a3",
    FLG = ?SUPPORTS,    % from library if based on Config
    #impl{b_version = BEV, flags = FLG, handle = HND, lib = LIB}.


-spec start(#impl{}, riak_backend:config()) -> #inst{} | riak_backend:error().
start(#impl{flags = FLG} = Impl, _Config) ->
    HND = <<"reference-obtained-from-impl-handle">>,
    LOC = <<"where-the-data-is-stored">>,
    #inst{handle = HND, data_path = LOC, flags = FLG, impl = Impl};
start(Impl, Config) ->
    erlang:error('badarg', [Impl, Config]).


-spec stop(#inst{}) -> 'ok' | riak_backend:error().
stop(#inst{} = _Inst) ->
    'ok';
stop(Inst) ->
    erlang:error('badarg', [Inst]).


-spec supports() -> riak_backend:features().
supports() ->
    ?SUPPORTS.


-spec supports(#impl{} | #inst{}) -> riak_backend:features() | no_return().
supports(#impl{flags = FLG}) ->
    FLG;
supports(#inst{flags = FLG}) ->
    FLG;
supports(Arg) ->
    erlang:error('badarg', [Arg]).

