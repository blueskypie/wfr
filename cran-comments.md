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
Seems the concern from the Good Practice is that **People do often forget to detach databases**, which clearly **is not** the case here. The `attach` is closely followed by a `detach`. So I hope it is OK.

## Why I need to attach a workspace image just to read one object
The function `saveOutput` in my package has a Boolean parameter `saveWorkspace`. If it's TRUE, the workspace image is saved; otherwise, only the _output_ is saveRDS-ed. Although other functions, e.g. the `showObj` which contains those three lines of code, only need the _output_, the users may want to keep the workspace image, where the _output_ is produced, for their OWN interest, that's why I provided the parameter 'saveWorkspace'.

Function `showObj` reads in the _output_. If `saveWorkspace = T` in `saveOutput`, only the workspace image is saved, not the individual _output_ object. Therefore `showObj` needs to `attach` the saved workspace image to read the "OBJ2", the name of the _output_ object. 

To avoid using `attach` if `saveWorkspace = T`, function `showObj` could save both the workspace AND separately the _output_ object. But I feel that is logically and physically redundant, it is like the _output_ object is saved in two copies.

