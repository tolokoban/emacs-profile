# emacs-profile
This is just my emacs profile with all my snippets and custom modes

Put this in your `~/.emacs` file to apply this profile:

```
(setq root "~/Code/github/emacs-profile/")
(load-file (concat root "emacs-init.el"))
```

# Adding ESLint support

First install ESLint and Babel:
```
npm install -g eslint babel-eslint
```

Install following packages with the EMacs package manager:
```
M-x list-packages
```

Packages to install:
* `flycheck`
* `exec-path-from-shell`
