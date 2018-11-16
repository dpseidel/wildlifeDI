# ---- roxygen documentation ----
#
#' @title Identify simultaneous fixes between trajectories
#'
#' @description
#'   The function \code{GetSimultaneous} identifies and extracts simultaneous fixes, 
#'   within a given tolerance limit, between two movement datasets.
#'
#' @details
#'   This function is used to determine the simultaneous fixes between two movement 
#'   datasets facilitating further analysis.
#'
#' @param traj1 an object of the class \code{ltraj} which contains the time-stamped
#'    movement fixes of the first object. Note this object must be a \code{type II
#'    ltraj} object. For more information on objects of this type see \code{help(ltraj)}.
#' @param traj2 same as \code{traj1}.
#' @param tc time threshold for determining simultaneous fixes. For simplicity, \code{tc}
#'    is always taken in seconds.
#'
#' @return
#' A single ltraj object containing two bursts, representing the two original \code{ltraj} 
#' objects, each containing only those fixes that are deemed simultaneous.
#'
# @references
#'
#' @keywords processing
#' @seealso as.ltraj
#' @examples
#' data(deer)
#' deer37 <- deer[1]
#' deer38 <- deer[2]
#' #tc = 7.5 minutes
#' trajs <- GetSimultaneous(deer37, deer38, tc = 7.5*60)
#' deer37 <- trajs[1]
#' deer38 <- trajs[2]
#' 
#' @export
#
# ---- End of roxygen documentation ----
GetSimultaneous <- function(traj1,traj2,tc=0){
  
  #check to see if traj1 is not a type II
  if (attributes(traj1)$typeII == FALSE) {stop("The traj1 object is not a TypeII ltraj object")}
  #check to see if traj2 is not a type II
  if (attributes(traj2)$typeII == FALSE) {stop("The traj2 object is not a TypeII ltraj object")}
  #store as dataframes
  tr1 <- ld(traj1)
  tr2 <- ld(traj2)
  #get the length of each trajectory
  n1 <- dim(tr1)[1]
  n2 <- dim(tr2)[1]
  #Get nearest fixes that are within tc from one another.
  match1 <- data.frame()
  for (i in 1:n1){
    matched <- which.min(abs(difftime(tr1$date[i],tr2$date,units="secs")))
    temp <- data.frame(tr1=i,tr2=matched,dt=abs(difftime(tr1$date[i],tr2$date[matched],units="secs"))  )
    match1 <- rbind(match1,temp)
  }
  match2 <- data.frame()
  for (i in 1:n2){
    matched <- which.min(abs(difftime(tr2$date[i],tr1$date,units="secs")))
    temp <- data.frame(tr1=matched,tr2=i,dt=abs(difftime(tr2$date[i],tr1$date[matched],units="secs"))  )
    match2 <- rbind(match2,temp)
  }
  
  match1 <- match1[which(match1$dt <= tc),]
  match2 <- match2[which(match2$dt <= tc),]
  
  ind <- NULL
  for (i in unique(match1$tr2)){
    ind1 <- which(match1$tr2 == i)
    ind1 <- ind1[which.min(match1$dt[ind1])]
    ind <- c(ind,ind1)
  }
  match1 <- match1[ind,]
  ind <- NULL
  for (i in unique(match2$tr1)){
    ind1 <- which(match2$tr1 == i)
    ind1 <- ind1[which.min(match2$dt[ind1])]
    ind <- c(ind,ind1)
  }
  match2 <- match2[ind,]
  
  ind.1 <- which(is.na(match(match1$tr1,match2$tr1))==TRUE)
  if (length(ind.1) > 0){ match1 <- match1[-ind.1,] }
  ind.2 <- which(is.na(match(match2$tr1,match1$tr1))==TRUE)
  if (length(ind.1) > 0){ match2 <- match2[-ind.2,] }
  
  tr1.sim <- tr1[match1$tr1,]
  tr2.sim <- tr2[match1$tr2,]
  
  #convert to ltraj objects
  out.traj1 <- dl(tr1.sim)
  out.traj2 <- dl(tr2.sim)
  
  #Return the two ltraj objects
  return(c(out.traj1,out.traj2))
}
#=============== End of GetSimultaneous Function ================================

#' getSim: A faster GetSimultaneous function
#' 
#' @param df1 a data frame which contains the time-stamped
#'    movement fixes of the first object. This function expects your dataframe to
#'    have columns `date`, `x`, and `y` - in the format of a ltraj data frame.  
#' @param df2 a data frame which contains the time-stamped
#'    movement fixes of the first object. 
#' @param tc time threshold for determining simultaneous fixes. For simplicity, \code{tc}
#'    is always taken in seconds. If NULL, this adapted function will calculate tolerane on the fly
#'    between 0 and 600 seconds. Intended for use with 15 min, 20 min, and 30 min, regular trajectories.
#' @return a list of two dataframe, each subset to the simultaneous timesteps shared between the dyad.  
#' @export

getSim <- function(df1, df2, tc = NULL) {
  
  if (!requireNamespace("lubridate", quietly = TRUE)) {
    stop("The lubridate package must be installed for this function to work. Please install it.",
         call. = FALSE
    )
  }
  
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("The dplyr package must be installed for this function to work. Please install it.",
         call. = FALSE
    )
  }
  
  # calculate tolerance on the fly for our clean trajectories.
  # this should ensure that we don't have to do further indexing.
  if (is.null(tc)) {
    # median because there irregularities, especially early
    fixes <- sort(c(median(df1$dt, na.rm = T), median(df2$dt, na.rm = T)))
    tc <- case_when(
      fixes[1] == fixes[2] ~ 0,
      identical(fixes, c(900, 1800)) ~ 0,
      identical(fixes, c(900, 1200)) ~ 300,
      identical(fixes, c(1200, 1800)) ~ 600
    )
  }
  
  # subset to only those sharing the same dates
  # should (in some cases) drastically reduce the datasets and thus memory issues
  matrix(c(range(df1$date), range(df2$date)), 2, 2)
  as.POSIXlt(apply(matrix(c(range(df1$date), range(df2$date)), 2, 2), 1, min),
             origin = "1970-01-01 00:00:00", tz = "UTC"
  )
  
  int1 <- lubridate::interval(start = min(df1$date), end = max(df1$date))
  int2 <- lubridate::interval(start = min(df2$date), end = max(df2$date))
  
  if (lubridate::int_overlaps(int1, int2) == FALSE) {
    return(NULL)
  }
  
  dates <- lubridate::intersect(int1, int2)
  tr1 <- df1[df1$date %within% dates, ]
  tr2 <- df2[df2$date %within% dates, ]
  
  n1 <- dim(tr1)[1]
  n2 <- dim(tr2)[1]
  
  min_diff <- function(t1, t2) {
    which.min(abs(difftime(t1, t2, units = "secs")))
  }
  
  id1 <- 1:n1
  diff1 <- sapply(tr1$date, min_diff, t2 = tr2$date) # this is the heavyweight
  dt1 <- mapply(tr1$date, diff1,
                FUN = function(tr, match) {
                  abs(difftime(tr, tr2$date[match], units = "secs"))
                }
  )
  
  id2 <- 1:n2
  diff2 <- sapply(tr2$date, min_diff, t2 = tr1$date) # this is the heavyweight
  dt2 <- mapply(tr2$date, diff2,
                FUN = function(tr, match) {
                  abs(difftime(tr, tr1$date[match], units = "secs"))
                }
  )
  
  # early returns to speed things up
  if (tc == 0) {
    return(list(tr1[which(dt1 <= tc), ], tr2[which(dt2 <= tc), ]))
  }
  
  match1 <- tibble(tr1 = id1, tr2 = diff1, dt = dt1)[which(dt1 <= tc), ]
  match2 <- tibble(tr1 = diff2, tr2 = id2, dt = dt2)[which(dt2 <= tc), ]
  
  if (nrow(match1) == nrow(match2)) {
    return(list(tr1[which(dt1 <= tc), ], tr2[which(dt2 <= tc), ]))
  }
  
  # the following still require more copying than i would like...
  # should vectorize some of this
  # thankfully it doesn't add significantly to memory requirements
  
  ind1 <- sapply(unique(match1)$tr2, function(x) {
    ind <- which(match1$tr2 == x)
    ind[which.min(match1$dt[ind])]
  })
  match1 <- match1[unique(ind1), ]
  
  ind2 <- sapply(unique(match1)$tr2, function(x) {
    ind <- which(match1$tr2 == x)
    ind[which.min(match1$dt[ind])]
  })
  match2 <- match2[unique(ind2), ]
  
  ind.1 <- which(is.na(match(match1$tr1, match2$tr1)) == TRUE)
  if (length(ind.1) > 0) {
    match1 <- match1[-ind.1, ]
  }
  ind.2 <- which(is.na(match(match2$tr1, match1$tr1)) == TRUE)
  if (length(ind.1) > 0) {
    match2 <- match2[-ind.2, ]
  }
  
  tr1.sim <- tr1[match1$tr1, c(1:3, 11)] # dropping ltraj cols as they could be wrong for new subset.
  tr2.sim <- tr2[match1$tr2, c(1:3, 11)]
  
  
  return(list(tr1.sim, tr2.sim))
}

