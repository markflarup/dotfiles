# README
If something doesn't work out of the box it will figure in this README

## Doom Emacs

### Packages

#### ox-jira
The option to export to JIRA seems to be gone after an update
([issue#56](https://github.com/stig/ox-jira.el/issues/56)). Fix this be loading
the library manually: `M-x load-library RET ox-jira RET`. It might be possible
to specify this package should be loaded after org-mode in the configuration to
avoid this manual step
