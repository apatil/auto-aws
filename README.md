# auto_aws

An automatically generated AWS client along the lines of [aws-go](https://github.com/stripe/aws-go).

## WIP warning

I'm writing this to learn OCaml and it's not even close to usable yet.

## Notes

* When possible, string-valued fields are converted to variant types. The following conversions are applied when necessary:
  * Enum options are capitalized, because variant constructors must begin with capital letters.
  * Periods are converted to underscores, 