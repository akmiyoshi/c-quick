gh auth login --hostname github.com --git-protocol https --web
cp ~/elisp/c-quick.el .
gh.exe release upload package c-quick.el --clobber
git-put.cmd
