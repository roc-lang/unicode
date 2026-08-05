## Test-only host effects.
Host := [].{
    ## Cumulative roc_alloc plus roc_realloc calls made since the Roc app began.
    ## Read immediately before and after a region and subtract to isolate it.
    alloc_count! : {} => U64
}
