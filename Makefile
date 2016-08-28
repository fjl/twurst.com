.PHONY: all publish sync clean

all: publish

publish:
	emacs --batch -l ~/.emacs.d/init.el -l publish-twurst.el -f publish-twurst.com

sync: publish
	rsync -rtvu --delete-delay _site fjl@twurst.com:/srv/www/twurst.com/source/

clean:
	rm -fr _site
