load_pkgs <- function(packages){
        # invisible(lapply(pkgs, function(x) if(!is.element(x, installed.packages()[, 1]))
        #         install.packages(x, repos = c(CRAN = "http://cran.rstudio.com"))))
        # invisible(lapply(pkgs, require, character.only = TRUE))
        # installed_packages <- pkgs %in% rownames(installed.packages())
        installed_packages <- packages %in% rownames(installed.packages())
        if (any(installed_packages == FALSE)) {
                install.packages(packages[!installed_packages])
        }
        invisible(lapply(packages, library, character.only = TRUE))
}