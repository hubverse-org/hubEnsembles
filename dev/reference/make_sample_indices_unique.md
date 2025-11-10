# Make the output type ID values of sample forecasts distinct for different models

Make the output type ID values of sample forecasts distinct for
different models

## Usage

``` r
make_sample_indices_unique(model_out_tbl)
```

## Arguments

- model_out_tbl:

  an object of class `model_out_tbl` with component model outputs (e.g.,
  predictions).

## Value

a model_out_tbl object with unique output type ID values for different
models but otherwise identical to the input model_out_tbl.

## Details

The new `output_type_id` column values will follow one of two patterns,
depending on whether the column is detected to be numeric:

1.  If the output type ID is not numeric (may be a character): A
    concatenation of the prediction's model ID and original output type
    ID

2.  If the output type ID is numeric: A numeric representation of the
    above pattern rendered as a factor.
