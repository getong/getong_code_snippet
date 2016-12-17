# Copyright 2012 Erlware, LLC. All Rights Reserved.
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

# =============================================================================
# Verify that the programs we need to run are installed on this system
# =============================================================================
REBAR=rebar3

.PHONY: all compile doc clean test dialyzer typer shell distclean pdf \
  clean-common-test-data rebuild xref elm elm-test r-test coffee




all: elm compile

elm:
	$(MAKE) $(MAKE_FLAGS) --directory elm elm
watch :
	$(MAKE) $(MAKE_FLAGS) --directory elm watch

# =============================================================================
# Rules to build the system
# =============================================================================




elm-test:
	$(MAKE) $(MAKE_FLAGS) --directory elm elm-test


eunit:
	$(REBAR) as test eunit


unit: eunit elm-test

qc: compile
	$(REBAR) as eqc eqc
dev:
	$(REBAR) as dev compile

compile:
	$(REBAR) as prod compile


doc:
	$(REBAR) edoc


test: compile eunit  elm-test dialyzer

shell: start

start:
	mkdir -p /tmp/traces
	$(REBAR) as dev  shell --sname wm

plt:
	$(REBAR) dialyzer --update-plt true --succ-typings false

dialyzer:
	$(REBAR) dialyzer --update-plt false


typer:
	typer --plt $(DEPS_PLT) -r ./src


xref:
	$(REBAR) xref


clean:
	$(MAKE) $(MAKE_FLAGS) --directory elm clean
	$(REBAR) clean --all
	rm -f *.beam
	rm -f /tmp/*.wmtrace

relclean: clean
	- rm -rf $(CURDIR)/_release


distclean: clean relclean
	- rm -rf $(CURDIR)/_build
	- rm -f boostrap/*.tar.gz

rebuild: distclean compile escript dialyzer test


release: elm coffee compile test
	mkdir -p _release
	$(REBAR) as prod release --output-dir _release

circle_release:
	mkdir -p _release
	$(REBAR) as prod release --output-dir _release

tar: release
	$(REBAR) as prod tar
	cp _build/prod/rel/squaretarget/*.tar.gz bootstrap


console:
	ERL_CRASH_DUMP=/dev/null ./_release/squaretarget/bin/squaretarget console


