# examples for twitter activity

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

downloadtweets <- function(searchfield, numtweets=10) {
  timeline <- searchTwitter(searchfield, n=numtweets)
  text <- gettext(timeline)
  screenname <- getscreenname(timeline)
  whencreated <- getwhencreated(timeline)
  return(data.frame(username=rep(searchfield, length(whencreated)),
    screenname=screenname,
    whencreated=as.POSIXlt(whencreated, origin="1970-01-01"),
    text=text))
}

