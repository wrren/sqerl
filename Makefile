PROJECT = sqerl
DEPS = cowboy emysql lager jsx
include erlang.mk
ERLC_OPTS += +'{parse_transform, lager_transform}'