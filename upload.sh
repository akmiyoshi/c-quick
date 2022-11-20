#! /usr/bin/env bash
gh auth login --hostname github.com --git-protocol https --web
cp -v ~/c-quick.el .
gh.exe release upload package c-quick.el --clobber
git-put.cmd
