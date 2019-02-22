# Sample-Cl-Bot

A sample bot by Common Lisp for Slack

## Usage

```lisp
> (ql:quickload :sample-cl-bot)
> (clack:clackup sample-cl-bot:*app* :port <port number>)
```

## Installation

1. Clone this project under a directory that can be recognized by quicklisp
2. Make a settings file
   1. Copy `settings.json.in` to `settings.json` 
   2. Rewrite a incoming web-hook url for Slack in `settings.json`

## Author

* eshamster (hamgoostar@gmail.com)

## Copyright

Copyright (c) 2016 eshamster (hamgoostar@gmail.com)
