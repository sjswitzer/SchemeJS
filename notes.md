A brief note about naming and string conventions:

Generally, variable, function and class names are camel case. An exception
is when a function is also a Scheme function with dashes in its name. In that case
the function uses an underscore where a dash occurs in the Scheme name.

Constants are all-caps. Strings are generally "str" if they are for humans; and 'str'
if they're for internal purposes; `str` is used when convenient, as it quite often is.
