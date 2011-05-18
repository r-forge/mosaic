# examples for twitter activity (Nick Horton @ USCOTS 2011 workshop)

require(twitteR)

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
  return(substr(as.character(time), 12, 13))
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
  return(data.frame(username=rep(searchfield, length(whencreated)),
    screenname=screenname, hour=hour,
    whencreated=whencreated, text=text))
}

ds1 <- downloadfollowers("williamshatner")
ds2 <- downloadfollowers("uscensusbureau")
ds <- rbind(ds1, ds2)



sas <- downloadtweets("#sas", n=200)
r <- downloadtweets("#rstats", n=200)
hours = c(sas$hour, r$hour)
system = c(rep("SAS", length(sas$hour)), rep("R", length(r$hour)))

xtabs(hours ~ system)

