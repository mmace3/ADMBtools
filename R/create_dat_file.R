#' Create .dat file for use in ADMB model
#'
#' `create_dat_file` takes a list of data `dat_list`,
#'
#' @param dat_list list with data that will be written to .dat file
#' @param d directory where .dat file will be placed. Default is the working directory.
#' @param f file name for .dat file. Default is ADMB.dat
#'
#' @details `dat_list` must be a `list`, if not an error is thrown.
#'
#' @export



create_dat_file <- function(dat_list, dat_name, meta, d = getwd(), f = "ADMB")
{

  if(class(dat_list) != "list")
    stop("dat_list must be a list")
  if(any(lapply(dat_list, FUN = mode) != "numeric"))
    stop("All elements of dat_list must be numeric")
  if(!dir.exists(d))
    stop("d must be a valid directory that already exists")
  if(any(grepl("[[:alnum:]]|_", strsplit(f, split = "")[[1]]) == FALSE))
    stop("The file name (f) must contain only letters, numbers, or _")


  file_name <- paste(d, "/", f, ".dat", sep = "")

  write(paste("#", "Input file for:", meta[[1]],  sep = " "), file = file_name)
  write(paste("#", "Created by:", meta[[2]], sep = " "), file = file_name, append = TRUE)
  write(paste("#", "Last modified:", date(), sep = " "), file = file_name, append = TRUE)
  write("", file = file_name, append = TRUE)

  n <- length(dat_list)

  for(i in 1:n)
  {
    write(paste("#", dat_name[[i]], sep = " "), file = file_name, append = TRUE)

    if(any(class(dat_list[[i]]) == "matrix"))
    {
      write.table(dat_list[[i]], file = file_name, append = TRUE, row.names = FALSE,
                  col.names = FALSE)
    } else
      {

        write(dat_list[[i]], file = file_name, append = TRUE)

      }

  }

  return(invisible(NULL))

}
