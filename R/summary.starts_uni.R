## File Name: summary.starts_uni.R
## File Version: 0.29

#############################################################
summary.starts_uni <- function( object , digits=3 , file=NULL, print_call=TRUE, ... )
{
    # open sink
    CDM::osink( file = file , suffix = paste0( "__SUMMARY.Rout") )
	
	#-- package versions
	cat("-----------------------------------------------------------------\n")
	sirt::sirt_summary_print_package(pack="STARTS")
	sirt::sirt_summary_print_package(pack="LAM")
	sirt::sirt_summary_print_rsession()
	# cat("\n")
	
	cat( object$description , "\n\n")

	cat( "Estimator =" , object$estimator , "\n" )
	cat( "Used function =" , object$used_function , "\n\n" )
	
	#-- print call
	if (print_call){
		sirt::sirt_summary_print_call(CALL=object$CALL)				
	}
	   
	#-- print computation time
	starts_summary_print_computation_time(object=object)
	
	if (object$use_pmle){
		cat( "Optimization function =" , object$fit_LAM$optim_fct , "\n" )
		cat( "Convergence code =" , object$fit_LAM$results_optim$convergence , "\n" )
		cat( "CONVERGED =" , object$fit_LAM$converged , "\n" )
	}
	
	if (object$use_amh){
		cat( "Number of burnin iterations =" , object$fit_LAM$n.burnin , "\n" )			
		cat( "Number of iterations =" , object$fit_LAM$n.iter , "\n" )					
		cat( "Number of saved iterations =" , object$fit_LAM$n.saved , "\n\n" )			
	}

    cat("-----------------------------------------------------------------\n")
    cat( "Deviance = " , round( object$deviance , 2 ) , "\n" )
    cat( "Log Likelihood = " , round( - object$deviance / 2 , 2 ) , "\n" )	
	if ( object$estimator %in% c("PML") ){
		cat( "Log Prior = " , round( object$ic$prior , 2 ) , "\n" )	
		cat( "Log Posterior = " , round( object$ic$post , 2 ) , "\n\n" )	
	}
	
    cat( "Number of persons = " , object$ic$n , "\n" )    
    cat( "Number of estimated parameters = " , object$ic$np , "\n\n" )    
						
	#-- print information criteria
	starts_summary_print_ic(object=object)
	
	#-- prior summary
	if ( object$estimator %in% c("PML","MCMC") ){	
		cat("-----------------------------------------------------------------\n")
		cat("Prior Summary \n")	
		obji <- object$fit_LAM$prior_summary	
		print(obji)
	}

		
	cat("-----------------------------------------------------------------\n")
	cat("Parameter Summary \n")	

	if (object$use_pmle){ 
		obji <- object$fit_LAM$pmle_summary 
	}
	if (object$use_amh){ 
		obji <- object$fit_LAM$amh_summary 
		vars <- c("parameter","MAP","SD", "Q2.5", "Q97.5" , "Rhat","SERatio",
					"effSize" , "accrate")		
		obji <- obji[-1,vars]
		obji$effSize <- round( obji$effSize )		
	}			
	sirt::sirt_summary_print_objects(obji=obji, from=2, digits=3, rownames_null=TRUE)	

	cat("-----------------------------------------------------------------\n")
	cat("Variance Proportions \n")	

	if (object$use_pmle){ 
		obji <- object$var_prop
	}
	if (object$use_amh){ 
		obji <- object$var_prop
		obji <- obji[, intersect(vars, colnames(obji)) ]
		obji$effSize <- round( obji$effSize )		
	}			
	sirt::sirt_summary_print_objects(obji=obji, from=2, digits=3, rownames_null=TRUE)	
			
	#--- model fit
	summary_starts_uni_print_model_fit(object=object)	

	# close sink
    CDM::csink( file = file )		
	
}
#############################################################	
