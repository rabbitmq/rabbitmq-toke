PACKAGE:=rabbitmq-toke
DEPS:=rabbitmq-server
TOKE_DIR:=toke
PRIV_DIR:=priv

include ../include.mk

clean::
	rm -rf $(PRIV_DIR)
	make -C $(TOKE_DIR) clean

update: toke
	(cd $(TOKE_DIR); hg pull || hg up -C default)

toke:
	hg clone http://hg.opensource.lshift.net/toke

toke_build:
	make -C $(TOKE_DIR)
	cp -r $(TOKE_DIR)/$(EBIN_DIR) .
	cp -r $(TOKE_DIR)/$(PRIV_DIR) .

$(DIST_DIR)/$(PACKAGE).ez: toke_build $(TARGETS)

.PHONY: toke_build