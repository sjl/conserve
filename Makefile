.PHONY: docs pubdocs test test-sbcl test-ccl test-ecl test-abcl

sourcefiles = $(shell ffind --full-path --literal .lisp)
docfiles = $(shell ls docs/*.markdown)
apidocs = $(shell ls docs/*reference*.markdown)
heading_printer = $(shell which heading || echo 'true')

# Testing ---------------------------------------------------------------------
test: test-sbcl test-ccl test-ecl test-abcl

test-sbcl:
	$(heading_printer) computer 'SBCL'
	sbcl --load test/run.lisp

test-ccl:
	$(heading_printer) slant 'CCL'
	ccl --load test/run.lisp

test-ecl:
	$(heading_printer) roman 'ECL'
	ecl --load test/run.lisp

test-abcl:
	$(heading_printer) broadway 'ABCL'
	abcl --load test/run.lisp

# Documentation ---------------------------------------------------------------
# $(apidocs): $(sourcefiles)
# 	sbcl --noinform --load docs/api.lisp  --eval '(quit)'

# docs/build/index.html: $(docfiles) $(apidocs) docs/title
# 	cd docs && ~/.virtualenvs/d/bin/d

# docs: docs/build/index.html

# pubdocs: docs
# 	hg -R ~/src/docs.stevelosh.com pull -u
# 	rsync --delete -a ./docs/build/ ~/src/docs.stevelosh.com/conserve
# 	hg -R ~/src/docs.stevelosh.com commit -Am 'conserve: Update site.'
# 	hg -R ~/src/docs.stevelosh.com push
