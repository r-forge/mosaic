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
downloadfollowers = function(username) {
  user = getUser(username) 
  followers = userFollowers(user)
  followername = getfollowername(followers)
  numtweets = getnumtweets(followers)
  numfriends = getnumfriends(followers)
  whencreated = getwhencreated(followers)
  return(data.frame(username=rep(username, length(followers)),
    followername=followername, numtweets=numtweets,
    numfriends=numfriends,
    whencreated=as.POSIXlt(whencreated, origin="1970-01-01")))
}
