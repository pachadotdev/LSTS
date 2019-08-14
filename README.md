# lsts

## Devtools check output

```
==> devtools::check()

Updating lsts documentation
Writing NAMESPACE
Loading lsts
Writing NAMESPACE
── Building ───────────────────────────────────── lsts ──
Setting env vars:
● CFLAGS    : -Wall -pedantic -fdiagnostics-color=always
● CXXFLAGS  : -Wall -pedantic -fdiagnostics-color=always
● CXX11FLAGS: -Wall -pedantic -fdiagnostics-color=always
─────────────────────────────────────────────────────────
✔  checking for file ‘/home/pacha/github/lsts/DESCRIPTION’ ...
─  preparing ‘lsts’:
✔  checking DESCRIPTION meta-information ...
─  installing the package to process help pages
   Loading required namespace: lsts
─  saving partial Rd database
─  checking for LF line-endings in source and make files and shell scripts
─  checking for empty or unneeded directories
─  building ‘lsts_1.1.tar.gz’
   
── Checking ───────────────────────────────────── lsts ──
Setting env vars:
● _R_CHECK_CRAN_INCOMING_USE_ASPELL_: TRUE
● _R_CHECK_CRAN_INCOMING_REMOTE_    : FALSE
● _R_CHECK_CRAN_INCOMING_           : FALSE
● _R_CHECK_FORCE_SUGGESTS_          : FALSE
── R CMD check ─────────────────────────────────────────────────────────────────
* using log directory ‘/home/pacha/github/lsts.Rcheck’
* using R version 3.6.1 (2019-07-05)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --as-cran’
* checking for file ‘lsts/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘lsts’ version ‘1.1’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking serialization versions ... OK
* checking whether package ‘lsts’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking for future file timestamps ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd line widths ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... WARNING
Undocumented arguments in documentation object 'Box.Ljung.Test'
  ‘z’ ‘lag’ ‘main’
Documented arguments not in \usage in documentation object 'Box.Ljung.Test':
  ‘y’ ‘x’ ‘n’ ‘s’ ‘p’ ‘spar.freq’ ‘spar.time’ ‘theta’ ‘phi’ ‘xlim’
  ‘ylim’ ‘zlim’ ‘ylab’ ‘palette.col’

Undocumented arguments in documentation object 'LS.whittle.loglik'
  ‘x’
Documented arguments not in \usage in documentation object 'LS.whittle.loglik':
  ‘start’

Undocumented arguments in documentation object 'block.smooth.periodogram'
  ‘N’ ‘S’
Documented arguments not in \usage in documentation object 'block.smooth.periodogram':
  ‘n’ ‘s’

Functions with \usage entries need to have the appropriate \alias
entries, and all their arguments documented.
The \usage entries must correspond to syntactically valid R code.
See chapter ‘Writing R documentation files’ in the ‘Writing R
Extensions’ manual.
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking examples ... OK
* DONE

Status: 1 WARNING
See
  ‘/home/pacha/github/lsts.Rcheck/00check.log’
for details.


── R CMD check results ─────────────────────────────────────────── lsts 1.1 ────
Duration: 19.5s

❯ checking Rd \usage sections ... WARNING
  Undocumented arguments in documentation object 'Box.Ljung.Test'
    ‘z’ ‘lag’ ‘main’
  Documented arguments not in \usage in documentation object 'Box.Ljung.Test':
    ‘y’ ‘x’ ‘n’ ‘s’ ‘p’ ‘spar.freq’ ‘spar.time’ ‘theta’ ‘phi’ ‘xlim’
    ‘ylim’ ‘zlim’ ‘ylab’ ‘palette.col’
  
  Undocumented arguments in documentation object 'LS.whittle.loglik'
    ‘x’
  Documented arguments not in \usage in documentation object 'LS.whittle.loglik':
    ‘start’
  
  Undocumented arguments in documentation object 'block.smooth.periodogram'
    ‘N’ ‘S’
  Documented arguments not in \usage in documentation object 'block.smooth.periodogram':
    ‘n’ ‘s’
  
  Functions with \usage entries need to have the appropriate \alias
  entries, and all their arguments documented.
  The \usage entries must correspond to syntactically valid R code.
  See chapter ‘Writing R documentation files’ in the ‘Writing R
  Extensions’ manual.

0 errors ✔ | 1 warning ✖ | 0 notes ✔
Error: R CMD check found WARNINGs
Execution halted

Exited with status 1.
```
