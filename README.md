YAPS (yet another password server)
==================================

This is a drop-in replacement for [yapm-server](https://github.com/marcusklaas/yapm-server).

[![CircleCI](https://circleci.com/gh/marcusklaas/yaps.svg?style=svg)](https://circleci.com/gh/marcusklaas/yaps)

A client for this API can be found at [elm-yapm-client](https://github.com/jordymoos/elm-yapm-client).

Getting started
---------------

Install stack:
```
curl -sSL https://get.haskellstack.org/ | sh
```
Clone the repo:
```
git clone https://github.com/marcusklaas/yaps.git
```
Enter the directory:
```
cd yaps
```
Build and install:
```
stack install
```
Copy the binary to some place you can access it, such as `/usr/lib/bin`. It should be at `~/.local/bin/yaps`.

Run the password server:
```
yaps /path/to/cert/file /path/to/key/file /path/to/encrypted/stuff port
```
The encrypted directory should be writable by yaps and should contain at least a passwords.txt and passhash.txt. Examples for these files can be found in the root of this repo. Use with password `changeme`.
