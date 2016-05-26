evaluateModel <- function(design, outcome, vars) {
	require(survey)
	varList <- unlist(lapply(1:length(vars),
							 function(i) combn(1:length(vars), i, simplify = FALSE)),
					  recursive = FALSE)
	expressions <- sapply(varList,
						  function(i) as.formula(paste(outcome, "~", paste(vars[i],collapse="+"))))
	results <- numeric(length(expressions))
	for (i in 1:length(expressions)) {
		message(paste("Testing expression ", i, " of ", length(expressions), sep = ""))
		model <- svyglm(expressions[[i]], design = design, family = binomial())
		results[i] <- AIC(model)[["AIC"]]
	}
	model <- svyglm(as.formula(expressions[[which.min(results)]]),
					design = design, family = quasibinomial())
	return(model)
}

