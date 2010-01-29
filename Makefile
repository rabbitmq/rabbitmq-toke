PACKAGE:=rabbitmq-toke
DEPS:=rabbitmq-server rabbitmq-erlang-client
TOKE_DIR:=toke
PRIV_DIR:=priv
EXTRA_TARGETS:=toke_build

include ../include.mk

clean::
	rm -rf $(PRIV_DIR)
	if [ -e $(TOKE_DIR)/Makefile ]; then make -C $(TOKE_DIR) clean; fi

update: toke
	(cd $(TOKE_DIR); hg pull; hg up -C default)

toke:
	hg clone http://hg.opensource.lshift.net/toke

toke_build: toke
	make -C $(TOKE_DIR)
	cp -r $(TOKE_DIR)/$(EBIN_DIR) .
	cp -r $(TOKE_DIR)/$(PRIV_DIR) .

.PHONY: toke_build
