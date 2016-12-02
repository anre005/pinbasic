## pinbasic v0.2.0 (Release Date: 2016-12-02)

### New Functions

* `simulateBS` for simulating daily buys and sells data
* `pin_confint`: computes confidence intervals for the probability of informed trading 

### Changes

* `pin_est_core`, `pin_est` and `qpin` gained two new argument: `confint` and `ci_control` 
* updated plotting structure for `qpin_plot`, facets are now grouped by probability parameters, 
  intensity parameters and the probability of informed trading
* `initial_vals` together with `method = "HAC_Ref"` now returns a number of sets of initial values depending 
  on `num_clust` argument, not only one set
* Vignette was added
    

## pinbasic v0.1.0 (Release Date: 2016-10-25)

* initial release
