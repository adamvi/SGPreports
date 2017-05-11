createRepo <- function(
	github.user.name,
	new.repo.name,
	repo.description=NULL,
	local.directory=getwd()
) {
	
	# require(curl) # no functions, just need libcurl installed on system
	if (local.directory != getwd()) {tmp.wd <- getwd(); setwd(local.directory)} else tmp.wd <- getwd()
	
	json <- paste0("{ \"name\":\"" , new.repo.name, "\"", if (!is.null(repo.description)) paste0(", \"description\":\"", repo.description, "\""), " }") #string we desire formatting
	if(.Platform$OS.type == "windows") {
		json <- shQuote(json , type = "cmd" )
		repo.cmd <- paste0("curl -i -u \"" , github.user.name, "\" https://api.github.com/user/repos -d \"" , json, "\"")
	} else repo.cmd <- paste0("curl -u \"", github.user.name, "\" https://api.github.com/user/repos -d \"", json, "\"")
	
	system(cmd1)
	
	# system(paste0("curl -u '", github.user.name, "' https://api.github.com/user/repos -d '{\"name\":\"", new.repo.name, "\"}'"))
	system(paste0("git remote add origin git@github.com:", github.user.name, "/", new.repo.name, ".git"))
	system("git push origin master")
}