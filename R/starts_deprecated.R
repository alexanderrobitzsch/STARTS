## File Name: starts_deprecated.R
## File Version: 0.01
## File Last Change: 2017-08-25 11:06:55

starts_deprecated <- function( old, new, pkg="STARTS" )
{
	string <- paste0( "The function '" , old , "' is deprecated and will be\n",
				"removed in future versions of the " , pkg , " package.\n" ,
				"Use '" , new , "' instead.\n" )
	cat(string)
}
