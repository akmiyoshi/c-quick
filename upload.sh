#! /usr/bin/env bash
cwd=`pwd`
gh auth login --hostname github.com --git-protocol https --web
cp -v ~/c-quick.el .
ts=`date "+%Y.%m%d.%H%M.%S"`
version="${ts}"
sed -i -e "s/;; Version:.*/;; Version: v${version}/g" c-quick.el
gh.exe release upload package c-quick.el --clobber
git-put.cmd
