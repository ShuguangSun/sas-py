[![MELPA](https://melpa.org/packages/sas-py-badge.svg)](https://melpa.org/#/sas-py)
[![MELPA Stable](https://stable.melpa.org/packages/sas-py-badge.svg)](https://stable.melpa.org/#/sas-py)
[![Build Status](https://github.com/ShuguangSun/sas-py/workflows/CI/badge.svg)](https://github.com/ShuguangSun/sas-py/actions)
[![License](http://img.shields.io/:license-gpl3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0.html)


# sas-py

Equip SAS in Emacs with [SASPy](https://github.com/sassoftware/saspy).

It runs an interactive python shell and wraps the utilities from SASPy to make
it convenient to work with SAS for any kind of SASPy supported SAS deployments,
for example, IOM for local installed SAS on Windows, IOM for remote workspace
server (e.g. SAS OA), and HTTP for SAS Viya. For more access method, please
refer to the documents of
[SASPy](https://sassoftware.github.io/saspy/configuration.html#choosing-an-access-method).



## Pre-installation

Install [SASPy](https://github.com/sassoftware/saspy) in Python

```
pip install saspy
```

It requires Emacs package `ESS` as well.

## Installation

Clone this repository. Add the following to your `.emacs`:

``` elisp
(require 'sas-py)
```

## Customization

| Customization             | Description                                                                     |
|---------------------------|---------------------------------------------------------------------------------|
| sas-py-cfgname            | cfgname. 'default' or the one defined in 'sascfg_personal.py', e.g. 'win_local' |
| sas-py-results-format     | "TEXT", "HTML" or "Pandas"                                                      |
| sas-py-batchp             | Sets the batch attribute for the SASsession object or not                       |
| sas-py-remote-name-ip-map | A pair of server name and IP address                                            |


## Command

| Command                  | Description                                                               |
|--------------------------|---------------------------------------------------------------------------|
| sas-py-python-init       | Init the SASPy Session                                                    |
| sas-py-submit-file       | Submit SAS code file                                                      |
| sas-py-submit-region     | Submit region with temporary SAS code file                                |
| sas-py-submit-to-pyshell | Submit to interactive python shell without create temporary SAS code file |
| sas-py-submit-in-context | Submit in contex, ie, in a separate SAS session                           |
| sas-py-grep-log          | Grep the log file for error or warinings                                  |
| sas-py-disconnect        | Disconnect                                                                |
| sas-py-reconnect         | Reconnect                                                                 |
| sas-py-endsas            | End SAS session                                                           |
| sas-py-lastlog           | Print lastlog                                                             |
| sas-py-assigned_librefs  | Print assigned librefs                                                    |
| sas-py-list_tables       | Print the list of tables in a library (libref)                            |
| sas-py-datasets          | Print the list of datasets in a library (libref) using `PROC DATASETS`    |
| sas-py-get-dataset       | Get a dataset                                                             |
| sas-py-lib_path          | Print lib_path of the libref                                              |
| sas-py-saslog            | Print the full saslog                                                     |
| sas-py-set_results       | Set the format of results                                                 |
| sas-py-set_batch         | Toggle batch mode                                                         |
| sas-py-submit-string     | Submit a string or a piece of SAS code                                    |
| sas-py-getwd             | Print current working directory                                           |
| sas-py-setwd             | Change current working directory                                          |
| sas-py-list-options      | List SAS options                                                          |
| sas-py-list-macro-vars   | List macro variables                                                      |
| sas-py-sascfg            | Show the config file used in this session                                 |
| sas-py-list_configs      | List config files                                                         |
| sas-py-data-describe     | Show describe/means of sas data                                           |


Note:
- 'reconnect' is not stable and need to be improved in this package.
- `sas-py-data-describe` need to be improved.
