
patch <- paste0(patch, sym)
if(!file.exists(patch)){
	dir.create(file.path(patch))
}

mainDir <- paste(patch, tf, sep = "/")
if(!file.exists(mainDir)){
	dir.create(file.path(mainDir))
	}

#patch_com <- paste0(patch_com, sym)
#if(!file.exists(patch_com)){
#	dir.create(file.path(patch_com))
#	}

subDir <- paste("Test", Sys.Date(), sep = "_");
dayDir <- paste0(mainDir, "/", subDir)
if(!file.exists(dayDir)){
	dir.create(file.path(dayDir))
	}
setwd(file.path(dayDir));



# subDir <- paste("Test", Sys.Date(), sep = "_");
# patch <- paste0(patch, sym)
# if(!file.exists(patch)){dir.create(file.path(patch))}
# mainDir <- paste(patch, tf, sep = "/")
# if(!file.exists(mainDir)){dir.create(file.path(mainDir))}
# if(!file.exists(file.path(mainDir, subDir))){
#   dir.create(file.path(mainDir, subDir))
# }
# setwd(file.path(mainDir, subDir));

