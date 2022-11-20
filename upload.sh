#! busybox bash
#gh auth login --hostname github.com --git-protocol https --web
#cp -v ~/elisp/c-quick/c-quick.el .
cp -v ~/c-quick.el .
gh.exe release upload package c-quick.el --clobber
git-put.cmd
#rm -rf ~/elisp/c-quick
