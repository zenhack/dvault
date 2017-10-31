dvault: dead simple password manager

# The Bumf

* Simple
* Stores passwords encrypted with your GPG key
* Stores one password per file, which makes it synchronization tool
  friendly.

# Basic operation

To add a password, run:

    $ gpg -ae > ~/.local/dvault/${tag}.pass.asc

...and type in the password.

Running `dvault` will:

* spawn `dmenu`, with a list of tags for passwords you've stored.
* decrypt the password you select with gpg
* copy it to the clipboard.
