# -------------------------------------------------------------------
#
# Copyright (c) 2016 Basho Technologies, Inc.
#
# This file is provided to you under the Apache License,
# Version 2.0 (the "License"); you may not use this file
# except in compliance with the License.  You may obtain
# a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.
#
# -------------------------------------------------------------------

prj_dir	:= $(CURDIR)
cache	:= $(prj_dir)/.cache

dl_tgts	:=
#
# tools
#
REBAR3	?= $(cache)/rebar3
ifneq ($(wildcard $(REBAR3)),)
rebar	:= $(REBAR3)
else
rebar	:= $(cache)/rebar3
dl_tgts	+= $(rebar)
endif

#
# how to download files if we need to
#
ifneq ($(dl_tgts),)
dlcmd	:= $(shell which wget 2>/dev/null || true)
ifneq ($(wildcard $(dlcmd)),)
dlcmd	+= -O
else
dlcmd	:= $(shell which curl 2>/dev/null || true)
ifneq ($(wildcard $(dlcmd)),)
dlcmd	+= -o
else
$(error Need wget or curl to download files)
endif
endif
endif

#
# The default edoc layout leaves the monospaced font tiny, so we simply
# append a tweak to the generated stylesheet.
#
cssfile := $(prj_dir)/doc/stylesheet.css
cssaddl := code,kbd,pre,tt { font-size: larger; }

.PHONY	: check clean clean-docs default docs prereqs veryclean

default : compile

prereqs ::

compile : prereqs
	$(rebar) as prod compile

clean : prereqs
	$(rebar) clean

docs : prereqs
	$(rebar) edoc
	@grep -q '$(cssaddl)' $(cssfile) || echo '$(cssaddl)' >> $(cssfile)

clean-docs :
	/bin/rm -rf $(prj_dir)/doc/*

veryclean :: clean-docs


check : prereqs
	$(rebar) as check do dialyzer, xref


veryclean :: clean
	/bin/rm -rf $(prj_dir)/_build

#
# downloads
#
prereqs :: $(dl_tgts)

ifneq ($(dl_tgts),)
veryclean ::
	/bin/rm -rf $(dl_tgts)
endif

$(cache)/rebar3 :
	@test -d $(@D) || /bin/mkdir -p $(@D)
	@echo Downloading $@ ...
	@$(dlcmd) $@ https://s3.amazonaws.com/rebar3/rebar3
	@/bin/chmod +x $@

