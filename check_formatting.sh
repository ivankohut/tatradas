#!/bin/bash

# Checks whether source code is properly formatted i.e. if source code complies to format defined by jcfsettings.xml
# Requirements
# - JEDI Code Format binary (jcf-cli) in PATH
# - clean working copy (no changes in local git repo)


# Check whether git working copy does not contain any changes
git status | grep "working tree clean"
if [ $? -ne 0 ]; then 
    echo "!!! GIT working directory not clean - code formatting check cannot run"
    exit 1
fi

# Checking format of source code - reformat and see whether anything has changed
jcf-cli -clarify -inplace -y -config=jcfsettings.xml -R source
git status | grep "working tree clean"
if [ $? -ne 0 ]; then 
    echo "!!! Some files were modified by code formatting - review and commit them."
    exit 1
fi
