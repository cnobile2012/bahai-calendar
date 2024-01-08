#
# Development by Carl J. Nobile
#
include include.mk

TODAY		= $(shell date +"%Y-%m-%dT%H:%M:%S.%N%:z")
PREFIX		= $(shell pwd)
BASE_DIR	= $(shell echo $${PWD\#\#*/})
TEST_TAG	= # Define the rc<version>
PACKAGE_DIR	= $(BASE_DIR)-$(VERSION)$(TEST_TAG)
APP_NAME	= bahai_calendar
DOCS_DIR	= $(PREFIX)/docs
LOGS_DIR	= $(PREFIX)/logs
RM_REGEX	= '(^.*.pyc$$)|(^.*.wsgic$$)|(^.*~$$)|(.*\#$$)|(^.*,cover$$)'
RM_CMD		= find $(PREFIX) -regextype posix-egrep -regex $(RM_REGEX) \
                  -exec rm {} \;
PIP_ARGS	= # Pass variables for pip install.
TEST_PATH	= # The path to run tests on.

#----------------------------------------------------------------------
.PHONY	: all
all	: help

#----------------------------------------------------------------------
.PHONY: help
help	:
	@LC_ALL=C $(MAKE) -pRrq -f $(firstword $(MAKEFILE_LIST)) : \
                2>/dev/null | awk -v RS= \
                -F: '/(^|\n)# Files(\n|$$)/,/(^|\n)# Finished Make data \
                     base/ {if ($$1 !~ "^[#.]") {print $$1}}' | sort | grep \
                -E -v -e '^[^[:alnum:]]' -e '^$@$$'

.PHONY	: tar
tar	: clobber
	@(cd ..; tar -czvf $(PACKAGE_DIR).tar.gz --exclude=".git" \
          --exclude="__pycache__" $(BASE_DIR))

# $ make tests
# $ make tests TEST_PATH=tests.test_bases
# $ make tests TEST_PATH=tests/test_bases.py:TestBases.test_version
.PHONY	: tests
tests	: clobber
	@mkdir -p $(LOGS_DIR)
	@nosetests --with-coverage --cover-erase --cover-inclusive \
                   --cover-html --cover-html-dir=$(DOCS_DIR)/htmlcov \
                   --cover-package=$(PREFIX)/src $(TEST_PATH)
	@echo $(TODAY)

.PHONY	: sphinx
sphinx  : clean
	(cd $(DOCS_DIR); make html)

.PHONY	: install-dev
install-dev:
	pip install $(PIP_ARGS) -r requirements/development.txt

# To add a pre-release candidate such as 'rc1' to a test package name an
# environment variable needs to be set that setup.py can read.
#
# The command below will work with any package target below.
# make build-deb TEST_TAG=rc1
#
# For example a deb file might look like 'nc-bookkeeper-0.10rc1-amd64.deb'.
#


#----------------------------------------------------------------------
.PHONY	: clean clobber

clean	:
	$(shell $(RM_CMD))

clobber	: clean
#	@(cd $(DOCS_DIR); make clobber)
	@rm -rf build dist
#	@rm -rf $(LOGS_DIR)
