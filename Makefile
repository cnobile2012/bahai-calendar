#
# Development by Carl J. Nobile
#
include include.mk

TODAY		= $(shell date +"%Y-%m-%dT%H:%M:%S.%N%:z")
PREFIX		= $(shell pwd)
BASE_DIR	= $(shell basename $(PREFIX))
TEST_TAG	= # Define the rc<version>, this is done in include.mk
PACKAGE_DIR	= $(BASE_DIR)-$(VERSION)$(TEST_TAG)
APP_NAME	= bahai_calendar
DOCS_DIR	= $(PREFIX)/docs
LOGS_DIR	= $(PREFIX)/logs
RM_REGEX	= '(^.*.pyc$$)|(^.*.wsgic$$)|(^.*~$$)|(.*\#$$)|(^.*,cover$$)'
RM_CMD		= find $(PREFIX) -regextype posix-egrep -regex $(RM_REGEX) \
                  -exec rm {} \;
COVERAGE_FILE	= $(PREFIX)/.coveragerc
TEST_TAG	=
PIP_ARGS	= # Pass variables for pip install.
TEST_PATH	= # The path to run tests on.
export DEBUG

#----------------------------------------------------------------------
.PHONY	: all
all	: help

#----------------------------------------------------------------------
.PHONY	: help
help	:
	@LC_ALL=C $(MAKE) -pRrq -f $(firstword $(MAKEFILE_LIST)) : \
                2>/dev/null | awk -v RS= \
                -F: '/(^|\n)# Files(\n|$$)/,/(^|\n)# Finished Make data \
                     base/ {if ($$1 !~ "^[#.]") {print $$1}}' | sort | grep \
                -E -v -e '^[^[:alnum:]]' -e '^$@$$'

.PHONY	: tar
tar	: clobber
	@(cd ..; tar -cJvf $(PACKAGE_DIR).tar.xz --exclude=".git" \
          --exclude="__pycache__" --exclude=".pytest_cache" $(BASE_DIR))

# Run all tests
# $ make tests
#
# Run all tests in a specific test file.
# $ make tests TEST_PATH=tests/test_bases.py
#
# Run all tests in a specific test file and class.
# $ make tests TEST_PATH=tests/test_bases.py::TestBases
#
# Run just one test in a specific test file and class.
# $ make tests TEST_PATH=tests/test_bases.py::TestBases::test_version
.PHONY	: tests
tests	: clobber
	@rm -rf $(DOCS_DIR)/htmlcov
	@coverage erase --rcfile=$(COVERAGE_FILE)
	@coverage run --rcfile=$(COVERAGE_FILE) -m pytest --capture=tee-sys \
         $(TEST_PATH)
	@coverage report -m --rcfile=$(COVERAGE_FILE)
	@coverage html --rcfile=$(COVERAGE_FILE)
	@echo $(TODAY)

.PHONY	: flake8
flake8	:
	# Error on syntax errors or undefined names.
	flake8 . --select=E9,F7,F63,F82 --show-source
	# Warn on everything else.
	flake8 . --exit-zero

.PHONY	: sphinx
sphinx  : clean
	(cd $(DOCS_DIR); make html)

# To add a pre-release candidate such as 'rc1' to a test package name an
# environment variable needs to be set that setup.py can read.
#
# make build TEST_TAG=rc1
# make upload-test TEST_TAG=rc1
#
# Assuming the version is 0.1.0 and rc1
# The tarball would then be named badidatetime-0.1.0rc1.tar.gz
#
.PHONY	: build
build	: export PR_TAG=$(TEST_TAG)
build	: clobber
	@./config.py
	hatch build dist

.PHONY	: upload
upload	: build
	@./config.py
	twine upload --repository pypi dist/*

.PHONY	: upload-test
upload-test: export PR_TAG=$(TEST_TAG)
upload-test: build
	@./config.py
	hatch publish --repo test dist/*
#	twine upload --verbose --repository testpypi dist/*

#
# Installation
#

.PHONY	: install-dev
install-dev:
	@python -m pip install --upgrade pip
	@pip install $(PIP_ARGS) -r requirements/development.txt

#----------------------------------------------------------------------
.PHONY	: clean clobber

clean	:
	$(shell $(RM_CMD))

clobber	: clean
	@rm -rf dist badidatetime.egg-info
	@rm -rf $(DOCS_DIR)/htmlcov
	@rm -rf $(DOCS_DIR)/build
