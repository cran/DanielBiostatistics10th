# DanielBiostatistics10th 0.1.3
Improvement: ?predictiveValue now takes vector `prevalence` and returns vector `PVP` and `PVN`.  

Add S3 method: ?autoplot.predictiveValue

Change function names: old names `aggregate_z_test`, `aggregate_t_test`, `prop_test_CLT` and `aggregate_var_test` become ?aggregate_z, ?aggregate_t, ?prop_CLT and ?aggregate_var.

Change behavior: calculate confidence interval only, if `null.value` is missing, in ?prop_CLT, ?aggregate_z, ?aggregate_t and ?aggregate_var 

# DanielBiostatistics10th 0.1.2
Rename parameter `phat` of ?prop_test_CLT to `xbar`
Add function ?print_freqs and ?print_OE

# DanielBiostatistics10th 0.1.1
Remove package \pkg{raster} import.

# DanielBiostatistics10th 0.1.0
First release.
