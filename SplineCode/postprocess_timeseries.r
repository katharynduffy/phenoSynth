#!/usr/bin/env Rscript

## generate all transition date statistics
## the code used in this summary is the V1.0 release
## as listed on the phenocamr github page

## load argparse library for command line
## argument processing.
library(argparse)

## other required libraries
library(rjson)
#library(unixtools)

## load the phenocamr library
library(phenocamr)

## override a couple of function
source("./expand_phenocam.r")
#source("./contract_phenocam.r")

## setup command line options/args

# create parser object
parser <- ArgumentParser()

parser$add_argument("-v", "--verbose",
                    action="store_true",
                    default=FALSE,
                    help="Print extra output.")

parser$add_argument("-t", "--truncate",
                    action="store",
                    type="integer",
                    default=NULL,
                    help="Year to truncate data")

parser$add_argument("sitename", nargs=1,
                    help="PhenoCam Sitename")
parser$add_argument("roiname", nargs=1,
                    help="ROI Name, e.g. AG_1000")

args <- parser$parse_args()
verbose <- args$verbose
sitename <- args$sitename
roiname <- args$roiname
truncate <- args$truncate

## This shouldn't be necessary but I think was
## for functions not included in the phenocamr
## package like "snow_flag()"

## ## load all files necessary for processing
## ## basically the phenocamr R package
## files = list.files("./R",full.names = TRUE)
## for (i in files){
##   source(i)
## }

ARCHIVE_DIR = "/data/archive"

        
## Set working directory, where data is stored.
## Use an environment var to set directory.


## The original version of the script read in an processed
## a list of ROIs.  This version just does a single one.

## ## read in file which specifies the roi list of "good" data
## roilist = read.table(
##   '/data/Dropbox/Research_Projects/working/data_paper/data/roi_list.csv',
##   sep = ',',
##   header = TRUE
## )

## parse/reformat the roi_id
roiparts = strsplit(roiname, split = "_")
veg_type = roiparts[[1]][1]
roi_id = roiparts[[1]][2]



## sel = c(
##         # "smokylook",
##         # "turkeypointenf74",
##         # "harvardbarn2"
##         # "nationalcapital",
##         # "oakridge1"
##         # "twitchellalfalfa"
##         # "acadia",
##         # "alligatorriver",
##         # "barrow",
##         # "imcrkfen"
##         "imcrkridge0"
##         # "imcrkridge1",
##         # "imcrktussock"
##         # "turkeypointenf39",
##         # "uwmfieldsta"
##         )

## mysites = which(roilist$site %in% sel)

## copy data files to local directory

## data_record_1: site specific metadata files

json_file = sprintf("%s_meta.json", sitename)
system(sprintf('cp %s/%s/%s ./data_record_1/',
               ARCHIVE_DIR,
               sitename,
               json_file), wait=TRUE)
text_file = sprintf("%s_meta.txt", sitename)
system(sprintf('cp %s/%s/%s ./data_record_1/',
               ARCHIVE_DIR,
               sitename,
               text_file), wait=TRUE)

## read and save metadata values
metadata = fromJSON(file=file.path("./data_record_1",
                                   json_file))

## grab metadata
mat = metadata$phenocam_site$MAT_worldclim
lat = metadata$phenocam_site$lat
lon = metadata$phenocam_site$lon
if (verbose) {
    print("Site Metadata: ")
    print(paste0("  Lat: ", lat))
    print(paste0("  Lon: ", lon))
    print(paste0("  MAT: ", mat))
}

## data_record_2: ROI lists and masks
## ROI list files
if (verbose) {
    cat("-- copying data record 2\n")
}
system(sprintf('cp %s/%s/ROI/%s_%s_%s_roi.csv ./data_record_2/',
               ARCHIVE_DIR,
               sitename,
               sitename,
               veg_type,
               roi_id), wait=TRUE)

## ROI masks
system(sprintf('cp %s/%s/ROI/%s_%s_%s_*.tif ./data_record_2/',
               ARCHIVE_DIR,
               sitename,
               sitename,
               veg_type,
               roi_id), wait=TRUE)

## copy data_record_3: ROI image statistics file
if (verbose) {
    cat("-- copying data record 3\n")
}
roistats_file = sprintf("%s_%s_%s_roistats.csv",
                        sitename,
                        veg_type,
                        roi_id)

if ( is.null(truncate) ) {
    system(sprintf('/bin/cp %s/%s/ROI/%s ./data_record_3/',
                   ARCHIVE_DIR,
                   sitename,
                   roistats_file), wait=TRUE)
} else {
    cat("--- truncating data record 3\n")
    ## use a simple awk script to read the file and
    ## truncate
    cmd1 = sprintf('/bin/cat %s/%s/ROI/%s | ',
                   ARCHIVE_DIR,
                   sitename,
                   roistats_file)
    cmd2 = "/usr/bin/awk -f truncate_roistats.awk "
    cmd3 = sprintf('-v truncate=%s > ./data_record_3/%s',
                   truncate,
                   roistats_file)
    cmd = paste0(cmd1, cmd2, cmd3)
    if (verbose) {
        print(cmd)
    }
    system(cmd)
    
}

## loop over the frequency types
for (frequency in c(3, 1)) {

    if (verbose) {
        print(sprintf("Post-processing %d-day summary file.",
                      frequency))
    }
    
    ## set original path to summary file
    summary_file = sprintf('%s_%s_%s_%sday.csv',
                           sitename,
                           veg_type,
                           roi_id,
                           frequency)
    input_path = sprintf('%s/%s/ROI/%s',
                         ARCHIVE_DIR,
                         sitename,
                         summary_file)

    if (verbose) {
        print(paste0("Input File Path: ", input_path))
    }

    # both 3-day and 1-day timeseries go here
    output_dir = "./data_record_4"


    ## set output destination
    output_path = sprintf('%s/%s',
                          output_dir,
                          summary_file)
    if (verbose) {
        print(paste0("Output File Path: ", output_path))
    }

    if ( ! file.exists(input_path)) {
        if (verbose) {
            print(sprintf("Can't find file %s .", input_path))
            print("... skipping.")
        }
    }

    ## copy file from archive location
    file.copy(from=input_path, to=output_path, overwrite = TRUE)
    

    ## ## fill in snow flag data where available
    ## cat("-- Inserting snow flags! \n")
    ## snow_flag(dest_file)

    ## read in local summary file
    df = read_phenocam(output_path)

    ## expand the time series if the frequency = 3 days
    # if (frequency == 3) {
    #     cat("-- Expanding 3-day data set to 1-day frequency with padding\n")
    #     df = try(suppressWarnings(expand_phenocam(df,
    #                                               truncate=truncate,
    #                                               internal=TRUE)))
    # } else {
    #     df = try(suppressWarnings(expand_phenocam(df,
    #                                               truncate=truncate,
    #                                               internal=TRUE)))
    # }

    ## flagging outliers
    cat("-- Flagging outliers! \n")
    df = try(suppressWarnings(detect_outliers(df)),
             silent=TRUE)
    if(inherits(df, "try-error")){
      cat("--failed \n")
    }

    ## smoothing time series
    cat("-- Smoothing time series! \n")
    df = try(suppressWarnings(smooth_ts(df, internal=TRUE)),
             silent=TRUE)
    if(inherits(df, "try-error")){
      cat("--- failed \n")
    }

    ## calculate the phenology / transition dates
    ## USE THE EXPANDED FILE, OTHERWISE THE 3-DAY
    ## ESTIMATES WILL BE OFF
    cat("-- Estimating transition dates! \n")

    ## calculate phenophase transitions and write output file
    phenophase_dir = "./data_record_5/"
    phenophase_check = try(suppressWarnings(
        phenophases(data=df,
                    out_dir=phenophase_dir,
                    internal=FALSE)),
        silent=TRUE)

    ##
    ## truncate file to up and including truncate year
    ##
    
    ## read header of the file
    header = readLines(output_path, n=22)

    ## add the post-processing day and time to the header
    header = header[-length(header)]
    header = c(header,
               sprintf("## Final Processing Date: %s", Sys.Date()),
               sprintf("## Final Processing Time: %s", format(Sys.time(), "%H:%M:%S")),
               "#"
    )

    ## write out postprocessed file
    ## if (verbose) {
    ##     cat("-- writing temporary file.\n")
    ## }
    write_phenocam(df, out_dir=output_dir)

    ## ## contract the datafile into the output directory
    ## if (verbose) {
    ##     cat("-- contracting file to original size.\n")
    ## }
    ## suppressWarnings(contract_phenocam(file.path(tempdir(),
    ##                                              summary_file),
    ##                                    no_padding=TRUE,
    ##                                    out_dir=output_dir))
    
}


