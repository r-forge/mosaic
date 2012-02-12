# functions to help start, track, and grab results from an Xgrid using R
# Nicholas Horton, nhorton@smith.edu
# $Id: xgrid.R,v 1.11 2011/07/07 19:17:20 nhorton Exp $

xgrid = function(grid="localhost", numsim=20, ntask=1, 
   indir="input", outdir="output", param=1, Rcmd="runjob.R", auth="None", 
   outfile="RESULTS.rda", prefix="RESULT", throttle=9999, sleeptime=5, 
   verbose=FALSE) {
   # submit a group of jobs to the Xgrid, letting the grid deal with 
   # scheduling and load balancing
   # numsim is the total number of simulations to run
   # ntask is the maximum number of repetitions (tasks) each job will run
   # outdir is the directory to put output files
   # param is an optional parameter provided to the agent
   # Rcmd is the name of the R command to run on the agent
   # auth is the type of authorization ("Kerberos", "Password" or "None")
   # outfile is the filename to store results once collated
   # prefix is the string to prepend to the name of the file for individual job results
   # throttle is the maximum number of jobs to queue at any time
   # sleeptime is the number of seconds to wait between status requests
   # verbose controls whether to display xgrid commands

   numberofjobs = floor(numsim/ntask)    

   if (verbose==TRUE) {
      cat("numsim=", numsim, " ntask=", ntask, "numberofjobs=", 
         numberofjobs, "\n")
   }
   if (numberofjobs != ceiling(numsim/ntask)) {
      stop("numsim divided by ntask should be an integer!")
   }
   if (ceiling(numsim/ntask) < 2) {
      stop("must have at least 2 jobs!")
   }
   if (file_test("-d", indir) != TRUE) {
	  stop(paste("The directory '", indir, "' is not a directory!\n", sep=""))
   }
   if (file.access(outdir, 0) == -1) {
      if (system(paste("mkdir ", outdir, sep="")) != 0) {
         stop(paste("The directory '", outdir, "' can't be created!\n", sep=""))
      }
   } else if (file_test("-d", outdir) != TRUE) {
      stop(paste("The directory '", outdir, "' is not a directory!\n", sep=""))
   }
   

   # a vector indicating file numbering for results
   jobidentifier = 1:numberofjobs + 9999  

   # a vector that we can modify as jobs are queued
   pendingjobs = jobidentifier   	

   activejobs = c() # a vector of active Apple grid job numbers
   whichjob = 1     # the current job whose status is to be ascertained
   
   # first start to load up the grid
   while (length(activejobs) < throttle & length(pendingjobs) > 0) {
      activejobs = c(activejobs, xgridsubmit(grid, auth, indir, Rcmd, 
         ntask, param, paste(prefix, "-", pendingjobs[1], sep=""), 
         verbose))
      pendingjobs = pendingjobs[-1]
   }
                                             
   while (length(pendingjobs) > 0) {    # still more to queue up
      statusline = xgridattr(grid, auth, activejobs[whichjob], verbose)
      if (grepl('Failed', statusline)==TRUE) {
         stop("Ack: a job failed! Seek help immediately.")
         status = "Failed"
      }
      if (grepl('Finished', statusline)==TRUE) {
         xgridresults(grid, auth, activejobs[whichjob], outdir, verbose)
         xgriddelete(grid, auth, activejobs[whichjob], verbose)
         activejobs = activejobs[-whichjob]
         activejobs = c(activejobs, xgridsubmit(grid, auth, indir, Rcmd, 
            ntask, param, paste(prefix, "-", pendingjobs[1], sep=""), 
            verbose))  
         pendingjobs = pendingjobs[-1]
      } else {
         whichjob = ifelse(whichjob==length(activejobs), 1, whichjob + 1)
         Sys.sleep(sleeptime) 
      }
   }

   # wait for everything to finish up
   while (length(activejobs) > 0) {
      statusline = xgridattr(grid, auth, activejobs[whichjob], verbose)
      if (grepl('Failed', statusline)==TRUE) {
         stop("Ack: a job failed! Seek help immediately.")
         status = "Failed"
      }
      if (grepl('Finished', statusline)==TRUE) {
         xgridresults(grid, auth, activejobs[whichjob], outdir, verbose)
         xgriddelete(grid, auth, activejobs[whichjob], verbose)
         activejobs = activejobs[-whichjob]
         whichjob = ifelse(whichjob >= length(activejobs), 1, whichjob)

      } else {
         whichjob = ifelse(whichjob==length(activejobs), 1, whichjob + 1)
         Sys.sleep(sleeptime) 
      }
   }
      
   # start to collate results
   if (verbose==TRUE) {
      cat("should have ", ntask, "*", length(jobidentifier)," entries.\n")
   }
   # load first file (which consists of a data frame called "res0") 
   # then rename it
   res = readRDS(paste(outdir, "/", prefix, "-", jobidentifier[1], sep=""))
   # now load up the rest of the files
   for (i in 2:length(jobidentifier)) {
     res0 = readRDS(paste(outdir, "/", prefix, "-", jobidentifier[i], sep=""))
     res[((i-1)*ntask+1):(((i-1)*ntask+1)+ntask-1),] = res0
   }
   saveRDS(res, file=outfile)
   return(res)
}

xgriddelete = function(grid, auth, jobnum, verbose=FALSE) {
   command = paste("xgrid -h ", grid, " -auth ",auth, 
      " -job delete -id ", jobnum, sep="")
   if (verbose==TRUE) {
      cat(command, "\n")
   }
   retval = system(command, intern=TRUE)
}

xgridresults = function (grid, auth, jobnum, outdir, verbose = FALSE) {
  command = paste("xgrid -h ", grid, " -auth ", auth, 
    " -job results -so job.out -se job.err -out ", 
    outdir, " -id ", jobnum, sep = "")
  if (verbose == TRUE) { cat(command, "\n") }
  retval = system(command, intern = TRUE)
  if (max(grepl("error =", retval)) == 1) { # something bad happened?  
    stop("Ack: controller inaccessible! Seek help immediately.")
  } 
  else { return(retval) }
}

	
xgridattr = function (grid, auth, jobnum, verbose = FALSE) {
  command = paste("xgrid -h ", grid, " -auth ", auth, " -job attributes -id ", 
    jobnum, sep = "")
  if (verbose == TRUE) {
    cat(command, "\n")
  }
  retval = system(command, intern = TRUE)
  if (max(grepl("error =", retval)) == 1) { 
  # something bad happened? check later
    if (verbose == TRUE) { cat("unable to check on job.\n") }
    return(statusline = "Unknown")
  } else { return(statusline = retval[grep("jobStatus", retval)]) }
}


xgridsubmit = function(grid, auth, indir, Rcmd, ntask, param, 
   resfile, verbose=FALSE) {
   # submit a single job to the Xgrid, 
   # with three arguments (ntask, param and resfile)
   command = paste("xgrid -h ",grid," -auth ",auth, " -job submit -in ", indir, 
      " /usr/bin/R64 CMD BATCH --no-save --no-restore '--args ",
      ntask, " ", param, " ", resfile, "' ", Rcmd, " ", Rcmd, resfile, 
      ".Rout", sep="")   # arguments must be in single quotes!
   if (verbose==TRUE) {
      cat(command, "\n") 
   }
   retval = system(command, intern=TRUE)
   jobnum = chartr('{}jobIdentifr=;','               ', retval[2])
   jobval = as.numeric(jobnum)
   if (is.na(jobval)) { stop("error getting jobnumber")}
   else return(jobval)
}
