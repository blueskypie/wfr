# wfr 0.5.2

## minor change
* `createRmd` function
    * Lines containing R markdown section header in their first cell can be inserted into the Excel file pointed by `outputListFn`. Read reference manual for details.
    
## minor bug fix
* `myFlexTable` function
    * Fixed a bug when the table contains only one row.


# wfr 0.5.1

## minor change
* `saveOutput` function
    * If `obj` is not a table or figure
        * original: its RDS file and the workspace are saved.
        * now: only its RDS file is saved.


# wfr 0.5.0

## new features

* `saveOutput` function
    * The default value of parameter `oFileName` is removed. Now its value must be provided.
    * A new parameter `numberOutputFiles` is provided with default value `TRUE`. If `FALSE`, the output files will not be numbered.
