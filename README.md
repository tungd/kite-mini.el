
# Mini Kite Mode

Emacs integration for WebKit remote debug protocol. Inspired by [Kite][kite]
and [Wooky][wooky].

Debugging JavaScript in WebKit browser (Chrome) from Emacs is always fun.
There's a couple of packages implement that idea already: [Skewer][skewer],
[Kite][kite] and [Wooky][wooky] with different feature sets. However, due
to my specific workflow and needs, they are not suitable, so I decided to
wrote this.

* Skewer: I don't feel like injecting a custom script or running a web
  server within Emacs. Moreover, the simple evaluation requires JavaScript
  to be structured in a specific way and doesn't employ the WebKit
  Debuger's power, especially live updating scripts. I love this feature
  a lot since it can be used to redefine closures and event handlers.
  Browser agnostic is a nice thing to have, but it is not crucial for me.
* Kite: Is huge and full featured, but I do not need debugging, it is better
  suit for something like [realgud][realgud].
* Wooky: sounds perfect for me, except it doesn't support live updating.
* SwankJS: full-featured and browser agnostic, however it is complicated
  to setup and somewhat unreliable.

# Installation

* Install the package via [MELPA](http://melpa.milkbox.net/), or add the package's
  directory to `load-path`

Note: I test this only on Emacs 24.1 and above. Pull requests for supporting
other versions are welcomed.

# Usage

Open Chrome/Chromium with remote debugging enabled:

    chromium --remote-debugging-port=9222
    # Or
    open -a /Applications/Google\ Chrome.app --args "--remote-debugging-port=9222"

Require the package

    (require 'kite-mini)
    # Automatically Turn on the mode for your buffer of choice.
    (add-hook 'js-mode-hook (lambda () (kite-mini-mode t)))
    (add-hook 'css-mode-hook (lambda () (kite-mini-mode t)))

Connect:

    <M-x> km-connect

# Credits

About half of the code is taken from Kite.

# License

GPL

[kite]: https://github.com/jscheid/kite
[wooky]: https://github.com/katspaugh/wooky.el
[realgud]: https://github.com/rocky/emacs-dbgr
