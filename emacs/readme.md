ghl.el

Open current file from emacs buffer on http://github.com

### Requirements:

- pbcopy
- git and github account

You also need to define *ghl-organization* and *ghl-browser-cmd* in you emacs init file e.g
```lisp
(setq *ghl-organization* "fsf" )
```
```lisp
(setq *ghl-browser-cmd* "/usr/bin/open -a '/Applications/Google Chrome.app' '%s'")
```

### Usage:

    M-x gh-copy

Builds and copies url to clipboard based on file from emacs buffer

    M-x gh-open
Opens current file from emacs buffer in the browser
