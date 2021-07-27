#!/bin/R

library(data.table)

DT <- fread(list.files('rawdata', '^2021.*csv', full.names=TRUE))

# -- 1. Aggregate time
DT[, total_time := End - Start]
DT[, compute_days := fifelse(yday(End) == yday(Start), 1, 
    yday(End) - yday(Start))]
# parse the resource string into usage
DT[, c('billing', 'cpu', 'mem') := 
    tstrsplit(AllocTRES, ',|=', type.convert=TRUE)[seq(2, 6, 2)]]
DT[, mem := as.numeric(gsub('G', '', mem))]
DT[is.na(mem), mem := 60 * AllocCPUS]


# -- 2. Aggregate resources
usage_DT <- DT[, 
    .(hours_per_day=as.numeric(total_time, units='hours')/compute_days, 
        cpu, mem), 
    by=.(year_day=yday(Start))]

# -- 3. Aggeregate the resource usage further
summary_DT <- usage_DT[, 
    .(usage=mean(hours_per_day, na.rm=TRUE), 
        avg_cpus=mean(cpu, na.rm=TRUE), 
        avg_mem=mean(mem, na.rm=TRUE),
        num_jobs=.N),
    by=year_day]