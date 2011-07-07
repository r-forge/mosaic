# functions to help start, track, and grab results from an Xgrid using R
# Nicholas Horton, nhorton@smith.edu
# $Id: xgrid.R,v 1.8 2011/07/06 11:12:47 nhorton Exp $

xgrid = function(grid="mathgrid0.smith.edu", numsim=20, ntask=1, 
   outdir="output", param=1, Rcmd="runjob.R", auth="Kerberos", outfile="RESULTS.rda", 
   suffix="RESULT", throttle=20, sleeptime=15, verbose=FALSE) {
   # submit a group of jobs to the Xgrid, letting the grid deal with 
   # scheduling and load balancing
   # numsim is the total number of simulations to run
   # ntask is the maximum number of repetitions (tasks) per job
   offset = 101  # needs to be 3 digits to keep formatting clean
   alljobs = floor(numsim/ntask)
   if (verbose==TRUE) {
      cat("numsim=",numsim," ntask=",ntask,"alljobs=",alljobs, "\n")
   }
   if (alljobs != ceiling(numsim/ntask)) {
      stop("numsim divided by ntask should be an integer!")
   }
   if (ceiling(numsim/ntask)<2) {
      stop("must have at least 2 jobs!")
   }

   if (file.access(outdir, 0) == -1) {
      if (system(paste("mkdir ", outdir, sep="")) != 0) {
         stop(paste("The directory '", outdir, "' can't be created!\n", sep=""))
      }
   } else if (file_test("-d", outdir) != TRUE) {
      stop(paste("The directory '", outdir, "' is not a directory!\n", sep=""))
   }

   # process the jobs
   jobs = numeric(alljobs)
   jobidentifier = 1:alljobs+9999
   for (i in 1:length(jobs)) {
      jobs[i] = xgridsubmit(grid, auth, Rcmd, ntask, param,
         paste(suffix, "-", jobidentifier[i], sep=""), verbose)
   }
   if (verbose==TRUE) {
      cat("jobs=",jobs,"\n")
   }
   whichjob = 1
   while (length(jobs)>0) {
      if (verbose==TRUE) {
         cat("length of jobs=", length(jobs)," whichjob=", whichjob, "\n", sep="")
      }
      returnlist = checkonjob(whichjob, jobs, grid, outdir, auth, sleeptime, verbose)
      jobs = returnlist$jobs
      whichjob = returnlist$whichjob
   }
   return(collateresults(jobidentifier, ntask, outdir, outfile, suffix, verbose))
}

collateresults = function(jobidentifier, ntask, outdir, outfile, suffix, verbose) {
  # open up a file to figure out what we have to work with
  if (verbose==TRUE) {
    cat("should have ", ntask, "*", length(jobidentifier)," entries.\n")
  }
  load(paste(outdir,"/",suffix,"-",jobidentifier[1],sep=""))
  res = res0
  for (i in 2:length(jobidentifier)) {
     load(paste(outdir,"/",suffix,"-",jobidentifier[i],sep=""))
     res[((i-1)*ntask+1):(((i-1)*ntask+1)+ntask-1),] = res0
  }
  save(res, file=outfile)
  return(res)
}

xgriddelete = function(grid, auth, jobnum, verbose=FALSE) {
   command = paste("xgrid -h ",grid," -auth ",auth, 
      " -job delete -id ", jobnum, sep="")
   if (verbose==TRUE) {
      cat(command, "\n")
   }
   retval = system(command, intern=TRUE)
}

xgridresults = function(grid, auth, jobnum, outdir, verbose=FALSE) {
   command = paste("xgrid -h ",grid," -auth ",auth, 
      " -job results -so job.out -se job.err -out ", outdir," -id ", jobnum, sep="")
   if (verbose==TRUE) {
      cat(command, "\n")
   }
   retval = system(command, intern=TRUE)
}
	
xgridattr = function(grid, auth, jobnum, verbose=FALSE) {
   command = paste("xgrid -h ",grid," -auth ",auth, " -job attributes -id ", jobnum, sep="")
   if (verbose==TRUE) {
      cat(command,"\n")
   }
   retval = system(command, intern=TRUE)
   return(statusline = retval[grep('jobStatus', retval)])
}

xgridsubmit = function(grid, auth, Rcmd, ntask, param, resfile, verbose=FALSE) {
   # submit a single job to the Xgrid, 
   # with arguments "num" (numbering of results) and "ntask" (number of simulations)
   # the function calls the command "myscript" in the input directory, 
   # with three arguments (ntask, param and resfile)
   command = paste("xgrid -h ",grid," -auth ",auth,
      " -job submit -in input /usr/bin/R64 CMD BATCH --no-save --no-restore '--args ",
      ntask, " ", param, " ", resfile, "' ", Rcmd, " ", Rcmd, resfile, ".Rout", sep="")   # arguments must be in quotes!
   if (verbose==TRUE) {
     cat(command, "\n") 
   }
   retval = system(command, intern=TRUE)
   jobnum = chartr('{}jobIdentifr=;','               ', retval[2])
   jobval = as.numeric(jobnum)
   if (is.na(jobval)) { stop("error getting jobnumber")}
   else return(jobval)
}

   
checkonjob = function(whichjob, jobs, grid, outdir, auth, sleeptime, verbose=FALSE) {
   # command to check the status on a bunch of queued jobs
   # get the results from a previously completed and submitted job
   jobnum = jobs[whichjob]
   statusline = xgridattr(grid, auth, jobnum, verbose)
   if (grepl('Failed', statusline)==TRUE) {
       stop("Ack: a job failed! Seek help immediately.")
   }
   if (grepl('Finished', statusline)==TRUE) {
      # get the results from a previously completed and submitted job (and clean up afterwards)
      xgridresults(grid, auth, jobnum, outdir, verbose)
      xgriddelete(grid, auth, jobnum, verbose)
      nextjob = ifelse(whichjob==length(jobs), 1, whichjob)
      if (runif(1)<.1) nextjob=1	 # sometimes jump back to the beginning
      jobs = jobs[-whichjob]
   } else {
      Sys.sleep(sleeptime)
      nextjob = ifelse(whichjob==length(jobs), 1, whichjob+1)
   }
   return(list(jobs=jobs, whichjob=nextjob))
}


