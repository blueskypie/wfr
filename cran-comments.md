## Test environments
- win-builder (devel and release)
- local Windows 10 install, R 3.5.1
- CentOS Linux 7, R 3.6.0
- ubuntu 12.04 (on travis-ci), R 3.1.2
- Fedora Linux, R-devel, clang, gfortran
- Ubuntu Linux 16.04 LTS, R-release, GCC


## R CMD check results
There were no ERRORs or WARNINGs, only 1 NOTE due to new submission. 

## Addressing Reviewers' comments
- The title has been shortened to be less than 65 characters.
- Function documents has been updated to make it clear that the input and output file names can contain relative or absolute paths.
