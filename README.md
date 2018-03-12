[![Build Status](https://travis-ci.org/angusmoore/absgeoutils.svg?branch=master)](https://travis-ci.org/angusmoore/absgeoutils)
[![Coverage Status](https://coveralls.io/repos/github/angusmoore/absgeoutils/badge.svg?branch=master)](https://coveralls.io/github/angusmoore/absgeoutils?branch=master)

# absgeoutils
Utilities to make working with ABS statistical geographies easier

# Installation

Install the package using the R `devtools` package:
```
library(devtools)
install_github("angusmoore/absgeoutils")
```

You may need to first install the `devtools` package if you don't already have it (`install.packages("devtools")`).

Installing may fail if `devtools` cannot correctly determine your proxy server. If so, you'll get the following error message when you try to install:
```
Installation failed: Timeout was reached: Connection timed out after 10000 milliseconds
```
If you get this message, try setting your proxy server with the following command, and then running the install again:
```
httr::set_config(httr::use_proxy(curl::ie_get_proxy_for_url("http://www.google.com")))
```
