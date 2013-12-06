PREFIX:=../
DAY=`date +%Y%m%d%H%M`
DEST:=$(PREFIX)$(PROJECT)

REBAR=./rebar
TMP_DIR=./dscp.tmp
APP_NAME=dscp
PROJECT_NAME=dscp


all:del_deps \
	get_deps \
	clean \
	compile

edoc:
	@$(REBAR) doc

test: clean \
	compile
	@$(REBAR) ct

clean:
	@$(REBAR) clean
	@rm -rf ./test/*.beam

compile:
	@$(REBAR) compile

dialyzer:
	@$(REBAR) dialyze

get_deps:	del_deps
	@$(REBAR) get-deps
	
del_deps:
	@rm -rf ./deps

update-deps:
	@$(REBAR) update-deps
test-compile:
	@erlc -I include  -W0 -DTEST=true -o ./ebin src/*.erl

test_suite:clean \
		compile
		@$(REBAR) ct suite=hurricane_app

release:
	rm -f $(PROJECT_NAME)*.zip
	rm -rf $(TMP_DIR)
	mkdir $(TMP_DIR)
	cp -r ./src $(TMP_DIR)/
	cp -r ./include $(TMP_DIR)/
	cp ./rebar $(TMP_DIR)/rebar
	cp ./rebar.config $(TMP_DIR)/rebar.config
	cd $(TMP_DIR);./rebar clean;./rebar compile;mkdir ./$(APP_NAME);mv ./ebin ./$(APP_NAME);mv ./include ./$(APP_NAME);mkdir ./rel;cd ./rel;../rebar create-node nodeid=$(PROJECT_NAME);cp ../../reltool.config ./
	cd $(TMP_DIR);./rebar generate
	cp ./app.config $(TMP_DIR)/rel/$(PROJECT_NAME)/etc
	cd $(TMP_DIR)/rel/$(PROJECT_NAME);find  ./lib/mr*/ebin/ -type f  -print |sort|xargs md5sum  > mr.$(DAY).md5;
	cd $(TMP_DIR)/rel;zip -r $(PROJECT_NAME)_for_test_`date +%m%d`$(BUILD_NUMBER)  ./$(PROJECT_NAME);cp $(PROJECT_NAME)*.zip ../../
	rm -rf $(TMP_DIR)
	
	

	
