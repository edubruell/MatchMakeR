SearchStrategy <- R6::R6Class("SearchStrategy",
                          public = list(
                            strategies = NULL,
                            weights = NULL,
                            
                            initialize = function() {
                              self$strategies <- list()
                              self$weights <- list()
                            },
                            add_strategy = function(field, functions, weight = 1) {
                              self$strategies[[field]] <- functions
                              self$weights[[field]] <- weight
                            },
                            
                            apply_strategies = function(data) {
                              for (field in names(self$strategies)) {
                                fn_chain <- self$chain_functions(self$strategies[[field]])
                                data[, (field) := fn_chain(.SD[[field]]), .SDcols = field]
                              }
                              return(data)
                            }
                            }