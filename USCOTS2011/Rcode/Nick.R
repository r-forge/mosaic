# examples for twitter activity (Nick Horton @ USCOTS 2011 workshop)
# Wed May 18 10:18:08 EDT 2011


require(twitteR)
require(mosaic)

getnumtweets = function(userlist) { 
  return(sapply(userlist, function(x) x$getStatusesCount()))
}

getfollowername = function(userlist) { 
  return(sapply(userlist, function(x) x$getName()))
}

getnumfriends = function(userlist) { 
  return(sapply(userlist, function(x) x$getFriendsCount()))
}

getwhencreated = function(userlist) { 
  return(sapply(userlist, function(x) x$getCreated()))
}

getscreenname = function(userlist) { 
  return(sapply(userlist, function(x) x$getScreenName()))
}

gettext = function(userlist) { 
  return(sapply(userlist, function(x) x$getText()))
}

gethour <- function(time) {
  return(as.numeric(substr(as.character(time), 12, 13)))
}

gettimeofday <- function(hour) {
  ifelse(hour <= 17 & hour >= 8, "workday", "othertime")
}

downloadfollowers <- function(username) {
  user <- getUser(username) 
  followers <- userFollowers(user)
  followername <- getfollowername(followers)
  numtweets <- getnumtweets(followers)
  numfriends <- getnumfriends(followers)
  whencreated <- getwhencreated(followers)
  return(data.frame(username=rep(username, length(followers)),
    followername=followername, numtweets=numtweets,
    numfriends=numfriends,
    whencreated=as.POSIXlt(whencreated, origin="1970-01-01")))
}

downloadtweets <- function(searchfield, n=10, since="2011-01-01") {
  timeline <- searchTwitter(searchfield, n=n, since=since)
  text <- gettext(timeline)
  screenname <- getscreenname(timeline)
  whencreated <- as.POSIXlt(getwhencreated(timeline), origin="1970-01-01")
  hour <- gethour(whencreated)
  timeofday <- gettimeofday(hour)
  timeofday <- gettimeofday(hour)
  return(data.frame(searchfield=rep(searchfield, length(whencreated)),
    screenname=screenname, hour=hour, timeofday=timeofday,
    whencreated=whencreated, text=text))
}

# ds1 <- downloadfollowers("williamshatner")
# ds2 <- downloadfollowers("uscensusbureau")
# ds <- rbind(ds1, ds2)
# aggregate(numtweets ~ username, data=ds, FUN=mean)
# aggregate(numtweets ~ username, data=ds, FUN=sd)
# bwplot( numtweets ~ username, data=ds)
# bwplot( numfriends ~ username, data=ds)


# sas <- downloadtweets("#sas", n=200)
# r <- downloadtweets("#rstats", n=200)
# system <- rbind(sas, r)

# xtabs(~ timeofday + searchfield, system)

