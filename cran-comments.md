## R CMD check results
There were no WARNINGs. 

There was 1 ERROR and 1 NOTE:

ERROR 
* checking examples ... ERROR
  Running examples in ‘flowmalizr-Ex.R’ failed
  The error most likely occurred in:
  
 base::assign(".ptime", proc.time(), pos = "CheckExEnv")
 ### Name: group_v_group
 ### Title: 1v1 comparison
 ### Aliases: group_v_group
 
 ### ** Examples
 
 group_v_group(1, 4)
  Error in dplyr::filter(., !is.na(percentage_of_total)) : 
    object 'gg_sep' not found
  Calls: group_v_group ... <Anonymous> -> <Anonymous> -> <Anonymous> -> <Anonymous>
  Execution halted

NOTE
* checking R code for possible problems ... NOTE
  sep_groups: no visible binding for '<<-' assignment to ‘df_sep’
  visualize_groups: no visible binding for '<<-' assignment to ‘gg_sep’

## Downstream dependencies 
 There are currently no downstream dependencies for this package.
 
comments:

