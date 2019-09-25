## Test environments
- win-builder (devel and release)
- local Windows 10 install, R 3.5.1
- CentOS Linux 7, R 3.6.0
- ubuntu 12.04 (on travis-ci), R 3.1.2


## R CMD check results
There were no ERRORs or WARNINGs, but 2 NOTEs. 

One NOTE is due to new submission. Below is the other NOTE:

> checking R code for possible problems ... NOTE  
  Found the following calls to attach():  
  File ‘wfr/R/rmd.util.r’:  
    attach(rImageFn, name = "rImage", pos = 2)  
  See section ‘Good practice’ in ‘?attach’.

Here is the corresponding "Good Practice" section in [`attach`](https://www.rdocumentation.org/packages/base/versions/3.6.1/topics/attach):
> Good Practice
attach has the side effect of altering the search path and this can easily lead to the wrong object of a particular name being found. People do often forget to detach databases.

The NOTE is due to the following three consecutive lines of code in function `showObj`
```r
        attach(rImageFn,name="rImage", pos=2)
        obj1=get("OBJ2",pos = 2)
        detach(name="rImage", pos=2)
```
Seems the concern from the Good Practice is that **People do often forget to detach databases**, which is not the case here. The `attach` is closely followed by a `detach`. So I hope it is OK.
