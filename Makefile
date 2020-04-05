PROJECT = eshet
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

SP = 4

release ?= debug

RELX_CONFIG = $(CURDIR)/rel/$(release)/relx.config
RELX_OPTS = $(relx_opts) --sys_config $(CURDIR)/rel/$(release)/sys.config
RELX_OPTS += --vm_args $(CURDIR)/rel/$(release)/vm.args

SHELL_OPTS=-config rel/debug/sys.config -args_file rel/debug/vm.args
SHELL_DEPS = sync

DIALYZER_OPTS = -Werror_handling -Wrace_conditions -Wunmatched_returns -Wunderspecs
EUNIT_OPTS = verbose

DEP_PLUGINS = cowboy

include erlang.mk
