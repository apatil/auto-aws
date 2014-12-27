# Aws_client

An automatically generated AWS client along the lines of [aws-go](https://github.com/stripe/aws-go).

## Notes

* When possible, string-valued fields are converted to variant types. The following conversions are applied when necessary:
  * Enum options are capitalized, because variant constructors must begin with capital letters.
  * Periods are converted to underscores, 