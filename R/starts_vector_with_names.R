## File Name: starts_vector_with_names.R
## File Version: 0.01
## File Last Change: 2017-08-26 22:01:47

starts_vector_with_names <- function( vec , vec_names )
{
	names(vec) <- vec_names
	return(vec)
}

