## Test environments
* local R installation, R 4.0.2
* ubuntu 16.04 (on travis-ci), R 4.0.2
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Answering comments from Uwe Ligges 2021-03-15

The broken URL in the README was fixed.
The package description was rewritten to include more details.

## Answering comments from Uwe Ligges 2021-03-16

The description was updated and doesn't start with the package's name.

> Also: Is there some reference about the method you can add in the
> Description field in the form Authors (year) <doi:.....>?

No there's no doi, the package is not based on any published article, it takes 
inspiration from previous work credited at the end of the README but the bulk of 
the work is original and all the work lives in the package only at the moment.

## Answering comments from Gregor Seyer 2021-03-18

The title has been updated to be less than 65 characters

\Value was added for every exported function (doc was slightly reorganised too),
with relevant explanations regarding class and output meaning.
