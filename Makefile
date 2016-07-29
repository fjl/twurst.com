.PHONY: all publish clean

all: publish

publish:
	emacs --batch -l ~/.emacs.d/init.el -l publish-twurst.el -f publish-twurst.com

clean:
	rm -fr _site
