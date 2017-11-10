dvault: dead simple password manager

# The Bumf

* Simple
* Stores passwords encrypted with your GPG key
* Stores one password per file, which makes it synchronization tool
  friendly.

# Basic operation

To generate a random password:

    $ dvault gen $password_tag

where `$password_tag` is a name for the password. You may be prompted by
gpg to enter recipients; make sure one of them is you!

Running `dvault` with no arguments will:

* spawn `dmenu`, with a list of tags for passwords you've stored.
* decrypt the password you select with gpg
* copy it to the clipboard.

Passwords are stored gpg-encrypted at
`${HOME}/.local/dvault/${pasword_tag}.pass.asc`.
