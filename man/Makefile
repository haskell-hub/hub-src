all: hub.1.gz hub.5.gz

%.gz: %
	gzip -c $< >$@

%.html: %.xml
	xmlto -m manpage-normal.xsl  html $^
	mv index.html $@

%.1: %-1.xml
	xmlto -m manpage-normal.xsl  man  $^
	bin/fixup.sh $@

%.5: %-5.xml
	xmlto -m manpage-normal.xsl  man  $^
	bin/fixup.sh $@

%.xml: %.txt
	asciidoc -b docbook -d manpage $^
