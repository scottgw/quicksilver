SUBDIRS = condition mutex prodcons threadring

.PHONY: subdirs $(SUBDIRS)

subdirs: $(SUBDIRS)

$(SUBDIRS): 
	$(MAKE) -C $@

