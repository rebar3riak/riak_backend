%% -*- mode: erlang; erlang-indent-level: 4; indent-tabs-mode: nil -*-
%% -------------------------------------------------------------------
%%
%% Copyright (c) 2010-2016 Basho Technologies, Inc.
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
%% EDoc is kinda brain dead when it comes to documenting callbacks, so they're
%% all documented in the module description.  This puts the function docs ahead
%% of the data types, but such is life.  The callback headers are crafted to
%% work with the standard edoc link macros, making them a touch harder to read
%% in the source, but the goal is for the generated docs to look and work as
%% they should.
%%
%% @version 1
%% @doc The {@module} module specifies general storage backend operations.
%%
%%  Additional classes of functionality are defined in separate behaviors,
%%  including (but not limited to) {@link riak_backend_index} and
%%  {@link riak_backend_query}.
%%
%%  Backends are referenced as <em>implementations</em> and <em>instances</em>,
%%  each with their own type of handle and recognized configuration values.
%%
%%  An implementation <em>MAY</em> support additional behaviors, such as
%%  {@link application}, {@link gen_server}, etc.
%%  In these cases various callbacks must discriminate appropriately between
%%  the parameter types for each supported behavior. Since the {@link handle()}
%%  types are assumed to be sufficiently different from the types expected by
%%  OTP behaviors, intelligent pattern matching in function heads should be
%%  possible to assure reliable behavior.
%%
%% <h2><a name="functions">Callback Details</a></h2>
%%
%%  <h3 class="function"><a name="implements-0">implements/0</a></h3>
%%  <div class="spec">
%%      implements() -> InterfaceList
%%  <div class="spec">
%%      InterfaceList :: {@link interfaces()}
%%  </div></div>
%%  Returns the list of supported {@module}[...] interfaces.
%%
%%  The calling interface is always included in the result.
%%
%%  <h3 class="function"><a name="implements-1">implements/1</a></h3>
%%  <div class="spec">
%%      implements(Interface) -> API_Version | `false'
%%  <div class="spec">
%%      Interface :: {@link interface()} <br />
%%      API_Version :: {@link api_version()}
%%  </div></div>
%%  Reports whether the specified Interface is supported.
%%
%%  The result is the version of the interface API that is supported,
%%  or `false' if the specified interface is not in the list returned
%%  by {@link implements/0}.
%%
%%  <h3 class="function"><a name="init-1">init/1</a></h3>
%%  <div class="spec">
%%      init(Config) -> Handle | Error
%%  <div class="spec">
%%      Config :: {@link config()} <br />
%%      Handle :: {@link handle()} <br />
%%      Error :: {@link error()}
%%  </div></div>
%%  Initializes the backend with the specified configuration.
%%
%%  The resulting handle is passed to {@link start/2} to start one or more
%%  instances.
%%
%%  <h3 class="function"><a name="start-2">start/2</a></h3>
%%  <div class="spec">
%%      start(BackendHandle, Config) -> InstanceHandle | Error
%%  <div class="spec">
%%      Config :: {@link config()} <br />
%%      BackendHandle :: {@link handle()} <br />
%%      InstanceHandle :: {@link handle()} <br />
%%      Error :: {@link error()}
%%  </div></div>
%%  Starts an instance of the backend.
%%
%%  If `{error, already_running}' is returned, the {@link handle()} that
%%  is current for the instance should be used to perform operations.
%%
%%  <h3 class="function"><a name="stop-1">stop/1</a></h3>
%%  <div class="spec">
%%      stop(InstanceHandle) -> `ok' | Error
%%  <div class="spec">
%%      InstanceHandle :: {@link inst_handle()} <br />
%%      Error :: {@link error()}
%%  </div></div>
%%  Stops a backend instance.
%%
%%  <h3 class="function"><a name="supports-0">supports/0</a></h3>
%%  <div class="spec">
%%      supports() -> Features
%%  <div class="spec">
%%      Features :: {@link features()}
%%  </div></div>
%%  Reports the backend implementation's supported features.
%%
%%  <h3 class="function"><a name="supports-1">supports/1</a></h3>
%%  <div class="spec">
%%      supports(BackendHandle | InstanceHandle) -> Features | Error
%%  <div class="spec">
%%      BackendHandle :: {@link handle()} <br />
%%      InstanceHandle :: {@link handle()} <br />
%%      Features :: {@link features()}
%%      Error :: {@link error()}
%%  </div></div>
%%  Reports the implementation's or instance's supported features.
%%
%%  If invoked with a BackendHandle, the result <em>may</em> differ from
%%  that of {@link supports/0} if the loaded implementation has been
%%  configured to support a set of features other than its default.
%%
%% @end
-module(riak_backend).

-compile(no_auto_import).

%%======================================================================
%%  Public API
%%======================================================================

-export_type([
    api_version/0,
    attributes/0,
    be_version/0,
    bucket/0,
    config/0,
    error/0,
    feature/0,
    feature_create/0,
    feature_delete/0,
    feature_read/0,
    feature_readonly/0,
    feature_update/0,
    features/0,
    handle/0,
    interface/0,
    interfaces/0,
    key/0,
    not_sup/0,
    value/0
]).

%%======================================================================
%%  Types
%%======================================================================

-type api_version() :: pos_integer().
%% The version of a backend API, i.e. this interface.

-type be_version()  :: string().
%% The version of a backend implementation.  This is a freeform string, but
%% is expected to have a semver-ish format, possibly with a text suffix.
%% Extra points for separating the suffix from the dotted-decimal version
%% with a dash (`-').

-type feature_create() :: 'create'.
%% Instances can create new records.
%% If {@link put/4} or {@link put/5} is invoked with a new {@link key()} on
%% an instance that does not support this feature, {@link not_sup()} is
%% returned.

-type feature_delete() :: 'delete'.
%% Instances can delete existing records.
%% If {@link delete/3} is invoked on an instance that does not support this
%% feature, {@link not_sup()} is returned.

-type feature_read() :: 'read'.
%% Instances can read existing records.
%% If {@link get/3} is invoked on an instance that does not support this
%% feature, {@link not_sup()} is returned.

-type feature_readonly() :: 'readonly'.
%% Instances can apply a `read only' attribute to records.
%%
%% If supported, the attribute may be applied to `create' or `update'
%% operations.

-type feature_update() :: 'update'.
%% Instances can update (overwrite) existing records.
%% If {@link put/4} or {@link put/5} is invoked with an existing {@link key()}
%% on an instance that does not support this feature, {@link not_sup()} is
%% returned.

-type feature()     :: feature_create()
                     | feature_delete()
                     | feature_read()
                     | feature_readonly()
                     | feature_update().
%% An individual backend feature.

-type features()    :: [feature()].
%% A collection of backend features.

-type bucket()      :: term().
%% A backend storage bucket in whatever form the implementation supports.
%% Type constraints on buckets should be clearly documented by the specific
%% implementations should be clearly documented..

-type key()         :: term().
%% A backend record key in whatever form the implementation supports.
%% Type constraints on keys should be clearly documented by the specific
%% implementations should be clearly documented.

-type value()       :: term().
%% A backend record value in whatever form the implementation supports.
%% Type constraints on values should be clearly documented by the specific
%% implementation.

-type error()       :: {'error', term()}.
%% Unless otherwise specified, all errors are returned in standard
%% `{error, Reason}' form.

-type not_sup()     :: {'error', {'not_supported', feature()}}.
%% The specific error returned by backend functions invoked in a manner
%% requiring an unsupported {@link feature()}.  Callers should check
%% {@link supports/0} and/or {@link supports/1} to avoid this error.

-type config()      :: [{atom(), term()}].
%% The configuration used/required to initialize a backend or instance.
%% Recognized and required configuration elements MUST be documented by
%% specific backend implementations.

-type attributes()  :: term().
%% Some information understood by a backend that affects an operation.
%% Type constraints on attributes MUST be clearly documented by specific
%% implementations.

-type interface()   :: module().
%% An interface (behavior) module.

-type interfaces()  :: [interface()].
%% A collection of interfaces.

-type handle()      :: term().
%% An opaque object containing the state required to properly reference
%% a backend implementation or instance.  Handles are logically constant,
%% so they are reusable without being updated after function invocations.

%%  @end
%%======================================================================
%%  API functions
%%======================================================================
%%
%% All documented in the module doc header.
%%

-callback implements() -> interfaces().
-callback implements(interface()) -> api_version() | 'false'.
-callback init(config()) -> handle() | error().
-callback start(handle(), config()) -> handle() | error().
-callback stop(handle()) -> 'ok' | error().
-callback supports() -> features().
-callback supports(handle()) -> features() | error().
