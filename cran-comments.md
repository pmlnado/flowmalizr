## R CMD check results
There were no WARNINGs and no NOTEs. 

There was 1 ERROR:

ERROR 
* checking examples ... ERROR
  Running examples in ‘flowmalizr-Ex.R’ failed
  The error most likely occurred in:
  
 base::assign(".ptime", proc.time(), pos = "CheckExEnv")
 ### Name: group_v_group
 ### Title: 1v1 comparison
 ### Aliases: group_v_group
 
 ### ** Examples
 
 group_v_group(1, 3)
  Error in dplyr::filter(., !is.na(percentage_of_total)) : 
    object 'gg_sep' not found
  Calls: group_v_group ... <Anonymous> -> <Anonymous> -> <Anonymous> -> <Anonymous>
  Execution halted


## Downstream dependencies 
 There are currently no downstream dependencies for this package.
 
comments:
I have tried many work arounds to remove the error but cannot find a solution. However, the package works correctly and the solution might be simple. It looks like it might be coming from the running the example in the roxygen skeleton. 
