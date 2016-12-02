## Version 0.2.0

### New Functions

* `simulateBS` for simulating daily buys and sells data
* `pin_confint`: computes confidence intervals for the probability of informed trading 

### Changes

* updated plotting structure for `qpin_plot`, facets are now grouped by probability parameters, 
  intensity parameters and the probability of informed trading
* `initial_vals` together with `method = "HAC_Ref"` now returns a number of sets of initial values depending 
  on `num_clust` argument, not only one set
* Vignette was added
    

## Version 0.1.0

initial release