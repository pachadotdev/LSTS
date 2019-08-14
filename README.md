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
* checking R code for possible problems ... NOTE
Box.Ljung.Test: no visible global function definition for ‘acf’
Box.Ljung.Test: no visible binding for global variable ‘na.pass’
Box.Ljung.Test: no visible global function definition for ‘pchisq’
Box.Ljung.Test: no visible global function definition for ‘plot’
Box.Ljung.Test: no visible global function definition for ‘abline’
LS.kalman: no visible global function definition for ‘na.omit’
LS.kalman: no visible global function definition for ‘ARMAtoMA’
LS.summary: no visible global function definition for ‘pnorm’
LS.whittle: no visible global function definition for ‘nlminb’
LS.whittle.loglik: no visible global function definition for ‘na.omit’
block.smooth.periodogram: no visible global function definition for
  ‘smooth.spline’
block.smooth.periodogram: no visible global function definition for
  ‘colorRampPalette’
block.smooth.periodogram: no visible global function definition for
  ‘persp’
periodogram: no visible global function definition for ‘fft’
periodogram: no visible global function definition for ‘axis’
smooth.periodogram: no visible global function definition for
  ‘smooth.spline’
smooth.periodogram: no visible global function definition for ‘axis’
ts.diag: no visible global function definition for ‘sd’
ts.diag: no visible global function definition for ‘par’
ts.diag: no visible global function definition for ‘plot’
ts.diag: no visible global function definition for ‘abline’
ts.diag: no visible global function definition for ‘acf’
ts.diag: no visible binding for global variable ‘na.pass’
Undefined global functions or variables:
  ARMAtoMA abline acf axis colorRampPalette fft na.omit na.pass nlminb
  par pchisq persp plot pnorm sd smooth.spline
Consider adding
  importFrom("grDevices", "colorRampPalette")
  importFrom("graphics", "abline", "axis", "par", "persp", "plot")
  importFrom("stats", "ARMAtoMA", "acf", "fft", "na.omit", "na.pass",
             "nlminb", "pchisq", "pnorm", "sd", "smooth.spline")
to your NAMESPACE file.
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

Undocumented arguments in documentation object 'LS.kalman'
  ‘ar.order’ ‘ma.order’

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

Status: 1 WARNING, 1 NOTE
See
  ‘/home/pacha/github/lsts.Rcheck/00check.log’
for details.


── R CMD check results ─────────────────────────────────────────── lsts 1.1 ────
Duration: 19.4s

❯ checking Rd \usage sections ... WARNING
  Undocumented arguments in documentation object 'Box.Ljung.Test'
    ‘z’ ‘lag’ ‘main’
  Documented arguments not in \usage in documentation object 'Box.Ljung.Test':
    ‘y’ ‘x’ ‘n’ ‘s’ ‘p’ ‘spar.freq’ ‘spar.time’ ‘theta’ ‘phi’ ‘xlim’
    ‘ylim’ ‘zlim’ ‘ylab’ ‘palette.col’
  
  Undocumented arguments in documentation object 'LS.kalman'
    ‘ar.order’ ‘ma.order’
  
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

❯ checking R code for possible problems ... NOTE
  Box.Ljung.Test: no visible global function definition for ‘acf’
  Box.Ljung.Test: no visible binding for global variable ‘na.pass’
  Box.Ljung.Test: no visible global function definition for ‘pchisq’
  Box.Ljung.Test: no visible global function definition for ‘plot’
  Box.Ljung.Test: no visible global function definition for ‘abline’
  LS.kalman: no visible global function definition for ‘na.omit’
  LS.kalman: no visible global function definition for ‘ARMAtoMA’
  LS.summary: no visible global function definition for ‘pnorm’
  LS.whittle: no visible global function definition for ‘nlminb’
  LS.whittle.loglik: no visible global function definition for ‘na.omit’
  block.smooth.periodogram: no visible global function definition for
    ‘smooth.spline’
  block.smooth.periodogram: no visible global function definition for
    ‘colorRampPalette’
  block.smooth.periodogram: no visible global function definition for
    ‘persp’
  periodogram: no visible global function definition for ‘fft’
  periodogram: no visible global function definition for ‘axis’
  smooth.periodogram: no visible global function definition for
    ‘smooth.spline’
  smooth.periodogram: no visible global function definition for ‘axis’
  ts.diag: no visible global function definition for ‘sd’
  ts.diag: no visible global function definition for ‘par’
  ts.diag: no visible global function definition for ‘plot’
  ts.diag: no visible global function definition for ‘abline’
  ts.diag: no visible global function definition for ‘acf’
  ts.diag: no visible binding for global variable ‘na.pass’
  Undefined global functions or variables:
    ARMAtoMA abline acf axis colorRampPalette fft na.omit na.pass nlminb
    par pchisq persp plot pnorm sd smooth.spline
  Consider adding
    importFrom("grDevices", "colorRampPalette")
    importFrom("graphics", "abline", "axis", "par", "persp", "plot")
    importFrom("stats", "ARMAtoMA", "acf", "fft", "na.omit", "na.pass",
               "nlminb", "pchisq", "pnorm", "sd", "smooth.spline")
  to your NAMESPACE file.

0 errors ✔ | 1 warning ✖ | 1 note ✖
Error: R CMD check found WARNINGs
Execution halted

Exited with status 1.
```
