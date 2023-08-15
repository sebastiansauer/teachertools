# teachertools (development version)


# 0.3.0.9000


## Minor changes and bug fixes

* prep_csv: if only 1 column is present, but it is not named "id" or "pred", rename it to "pred" and add id variable

# 0.2.0.9000

Some small updates

## Minor changes and bug fixes

* the Magrittr pipe has been replaced by the base pipe in some instances (requires R >= 4.1)
* in `exam2yamlrmd` the type of the exercises (e.g. "num") is now ported as a yaml category
* in `render_exs` the option `testid = TRUE` was set for Moodle exercises
* `grading_scheme()` added
* `bonus_points()` added


# teachertools 0.1.0.9000

Initial version
