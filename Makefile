OUTDIR   = _site
LAYOUT   = layout.html
MARKDOWN = perl lib/Markdown.pl

.SILENT:
.PHONY: all clean

all:
	mkdir -p $(OUTDIR)
	$(call copy, favicon.ico)
	$(call copy, static)
	$(call mdown, index.markdown, $(LAYOUT), $(OUTDIR)/index.html)

clean:
	rm -frv $(OUTDIR) | sed -e 's/^/RM $0/'

define copy
	cp -vr $1 $(OUTDIR)
endef

define mdown
	@echo MDOWN $1 '>' $3
	awk '/{{content}}/ { system("$(MARKDOWN) $1"); next } /.*/' $2 > $3
endef
