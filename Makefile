#################### 
GROUP_NUMBER := 2
####################

ERLC := erlc
ERLC_FLAGS := -W -I include

ERL_FILES := $(wildcard src/*.erl)
BEAM_FILES := $(patsubst src/%.erl,ebin/%.beam,${ERL_FILES})

comma:= ,
empty:=
space:= $(empty) $(empty)

EDOC_SRC := $(filter-out %_test.erl, $(ERL_FILES))
EDOC_SRC_LIST := [$(subst $(space),$(comma),$(patsubst src/%.erl,'src/%.erl', $(EDOC_SRC)))]

REQUIRED_DIR_NAME := pop_2012_project_group_$(GROUP_NUMBER)

PROJECT_DIR := $(notdir $(shell pwd))

USER=$(shell whoami)
ARCHIVE_NAME :=  $(REQUIRED_DIR_NAME)_archive_$(USER)_$(shell date "+%Y-%m-%d__%H.%M.%S")__.tar.gz
ARCHIVE_DIR := ..

.PHONY: doc all clean start test archive remove_finderinfo

all: $(BEAM_FILES)

ebin:
	mkdir ebin

ebin/%.beam: src/%.erl ebin
	$(ERLC) $(ERLC_FLAGS) -o ebin $<

start: all
	(cd ebin && erl -eval 'foo:start(), init:stop()')

test: all
	(cd ebin && erl -noinput -eval 'eunit:test({dir, "."}, [verbose]), init:stop()')

doc:
	erl -noshell -eval "edoc:files($(EDOC_SRC_LIST), [{dir, 'doc/html/'}])" -s init stop

clean:
	rm -fr .#* *.dump
	rm -fr ebin/*.beam
	(find doc/html/ ! -name overview.edoc ! -name html -exec rm -rf {} \;)

remove_finderinfo:
	-xattr -d "com.apple.FinderInfo" src/*.erl include/*.hrl doc/*

archive: clean doc
ifeq ("$(REQUIRED_DIR_NAME)", "$(PROJECT_DIR)")
	cd $(ARCHIVE_DIR) && tar cvzf $(ARCHIVE_NAME) --exclude='.git*' $(PROJECT_DIR)
	@echo 	
	@echo NOTE: Archive created in $(ARCHIVE_DIR)/$(ARCHIVE_NAME)
	@echo 
else
	@echo "Error: Wrong directory name >$(PROJECT_DIR)<, change to >$(REQUIRED_DIR_NAME)<"
endif

