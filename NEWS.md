# multilex 1.0.1

# multilex 1.0.0

## Major changes

* [stringr](https://stringr.tidyverse.org/) is no longer a dependency, an now only base functions are used to deal with strings
* All dependencies are now listed using [roxigen2](https://github.com/r-lib/roxygen2) using the `#' @importFrom` syntax, instead of `package::function`
* README.md is now built from README.rmd 
* All functions use [rlang](https://rlang.r-lib.org/) `.data` and `.env` pronouns to access variables in pipes (this got rid of most CMD CHECK notes)
* Unicode characters are now handled by `replace_special_characters` using `chr_unserialise_unicode` from [rlang](https://rlang.r-lib.org/) (this also got rid of some CMD CHECK notes)
* `ml_connect` now uses `key_get` and `key_set` from  [keyring](https://github.com/r-lib/keyring) to store passwords
* Change `ml_connect` default host to [https://formr.org/admin/account/login]("https://formr.org/admin/account/login")

## Bug fixes

* Some participants' total degree of exposure to some languages exceeded 100%. Now they are fixed.
* `import_short` was using `languages_lockdown1` to compute degree of exposure, which led to some wrong scores, as "catalan_mallorca" was being used instead of "catalan_majorca" in this version, so now `import_short` uses its own languages vector, `languages_short`


