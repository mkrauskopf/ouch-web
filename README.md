OUCH web
========

Current version is a simple [Yesod]-based web application able to display basic information about molecules from
an uploaded [SMILES] file using [OUCH].


OUCH web in action :) (application screenshot)
----------------------------------------------
![OUCH web in action :)](https://github.com/mkrauskopf/ouch-web/raw/master/screenshot.png)


Installation
------------
In short using _cabal-dev_:

```sh
git clone git://github.com/odj/Ouch.git /local/path/to/Ouch
cabal-dev add-source /space/haskell/sources/Ouch
cabal-dev install
```

This will take some time on the first run.

Now you are ready to run the project with `yesod --dev devel`

Note that for _ouch-web_ installation [OUCH] package is needed. Since the OUCH package is not available via
Hackage it needs to be provided manually as shown above.


Truth behind
------------
So far this is mainly a playground for myself, Haskell, Yesod, Ouch and who knows what. Feel free to join ;)

[Yesod]: http://github.com/yesodweb/yesod
[SMILES]: http://en.wikipedia.org/wiki/Simplified_molecular-input_line-entry_system
[OUCH]: http://github.com/odj/Ouch

