#' This function just takes a list of caches, and loads them. It's designed
#' for stuff you already cached previously, so it won't build any caches.
#'
#' @param cacheNames Vector of caches to load.
#' @param ... Additional parameters passed to simpleCache.
#' @export
loadCaches =function(cacheNames, parallel=TRUE, ...) {
	for (i in 1:length(cacheNames)) {
		# By default, load these caches into the environment that
		# calls loadCaches (which is the grandparent, n=2, of the call to	
		# simpleCache.
		simpleCache(cacheNames[i], loadEnvir=parent.frame(n=2), ...);
	}
}

#' Loads caches in parallel; useful for loading several big caches simultaneously.
#' The other version doesn't work.
#'
#' @param cacheNames List of caches to load.
#' @param ... Additional parameters passed to simpleCache.
#' @param envir Environment to assign to; the typical simpleCache environment
#' assigner fails in parallel mode.
#' @export
#' @examples
#' simpleCache('test1', { 1 }, noload=TRUE)
#' simpleCache('test2', { 2 }, noload=TRUE)
#' simpleCache('test3', { 3 }, noload=TRUE)
#' rm(test1, test2, test3)
#loadCaches("test1")
#' e = loadCachesParallel(list("test1", "test2", "test3"))
#' e
#' names(e)
test1

loadCachesParallel = function(cacheNames, envir=parent.frame(n=2), ...) { 
	# If you use mclapply, the assign component of simpleCache doesn't work; you
	# lose the results because the assignments can't propogate through the job
	# splitting. Instead; mclapply passes results through the return value. So,
	# for this to work, I have to intercept the return value from lapply 
	# simpleCache and then do the assignments manually here.
	# We want preschedule = FALSE because the number of caches is low, 
	# and they might be too big for the long vector limitation.
	res = lapplyAlias(cacheNames, simpleCache, mc.preschedule=TRUE, ...)
	names(res) = cacheNames 
	for (cache in cacheNames) {
		assign(cache, res[[cache]], envir=envir)
	}
	return(res)
}


#' Show available caches.
#'
#' @param cacheSubDir Optional parameter to specify a subdirectory of the cache folder.
#' @export
availCaches =function(cacheSubDir="") {
	list.files(paste0(getOption("RCACHE.DIR"), cacheSubDir))
}


