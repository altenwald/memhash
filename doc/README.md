

# memhash #

Copyright (c) 2017 Altenwald Solutions, S.L.

__Authors:__ "Manuel Rubio" ([`manuel@altenwald.com`](mailto:manuel@altenwald.com)).

[![Build Status](https://img.shields.io/travis/altenwald/memhash/master.svg)](https://travis-ci.org/altenwald/memhash)
[![Codecov](https://img.shields.io/codecov/c/github/altenwald/memhash.svg)](https://codecov.io/gh/altenwald/memhash)
[![License: LGPL 2.1](https://img.shields.io/github/license/altenwald/memhash.svg)](https://raw.githubusercontent.com/altenwald/memhash/master/COPYING)

This project helps handling the memory for other implementations that requires some properties like a heterogeneous array with the use of references and keep all of them clean.


### <a name="Requirements">Requirements</a> ###

ePHP requires to be run over an Erlang/OTP 17+, but not all the versions are full compatible or recommended. See the list:

| Erlang Version | Support | Notes |
|:---|:---:|:---|
| 20.1 | :heavy_check_mark: | Recommended if you use OTP 20 |
| 20.0 | :heavy_check_mark: | |
| 19.3 | :heavy_check_mark: | Recommended if you use OTP 19 |
| 19.2 | :heavy_check_mark: | |
| 19.1 | :heavy_check_mark: | |
| 19.0 | :heavy_check_mark: | |
| 18.3 | :heavy_check_mark: | Recommended if you use OTP 18 |
| 18.2.1 | :heavy_check_mark: | |
| 18.2 | :heavy_check_mark: | |
| 18.1 | :heavy_check_mark: | |
| 18.0 | :heavy_check_mark: | |
| 17.5 | :heavy_check_mark: | Recommended if you use OTP 17 |
| 17.4 | :heavy_check_mark: | |
| 17.3 | :x: | fail in SSL |
| 17.1 | :heavy_check_mark: | |
| 17.0 | :heavy_check_mark: | |


### <a name="Installation">Installation</a> ###

A simple way to use, is include in your project `rebar.config` the following dependency line:

```erlang
    {memhash, {git, "git://github.com/altenwald/memhash.git", master}}
```


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="memhash.md" class="module">memhash</a></td></tr>
<tr><td><a href="memhash_data.md" class="module">memhash_data</a></td></tr></table>

