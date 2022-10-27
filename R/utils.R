#' Date selection
#'
#' automatically select common dates among C4R objects
#' @export
#' @importFrom transformeR bindGrid subsetDimension
#' @import dplyr
#' @param data list containing C4R objects, which are the outputs of the loadGridata function

common_dates <- function(data) {
  # list of models
  dates.all = c()

  for (imodel in 1:length(data)) {
    dates.all = c(dates.all, substr(data[[imodel]]$Dates$start, 1, 10))
  }

  dates.common = substr(data[[1]]$Dates$start, 1, 10)
  for (imodel in 2:length(data)) {
    aux = intersect(substr(data[[imodel - 1]]$Dates$start, 1, 10),
                    substr(data[[imodel]]$Dates$start, 1, 10))
    dates.common = intersect(dates.common, aux)
  }
  for (imodel in 1:length(data)) {
    ind = which(!is.na(match(
      substr(data[[imodel]]$Dates$start, 1, 10), dates.common
    )))
    data[[imodel]] = subsetDimension(data[[imodel]], dimension = "time", indices = ind)
  }

  return(bindGrid(data, dimension = "member"))

}


sign.prop <- function(array3d) {
  message("first dimension needs to be model member")

  if (length(dim(cl4.object$Data)) != 3)
    stop(
      "Your data needs to be a 3d array, with first dimension being model member. Check dimension"
    )
  find.sign = function(x) {
    signs = c(length(x[x < 0]) / length(x),
              length(x[x == 0]) / length(x),
              length(x[x > 0]) / length(x))
    names(signs) = c(-1, 0, 1)
    signs
  }

  #apply the find.sign function to the array
  array1_sign = apply(array3d, c(2, 3), find.sign)

  #####
  #create a function to find out which sign is the highest proportion, if there are ties, return NA
  find.most.sign = function(x) {
    if (length(x[x == max(x)]) == 1) {
      as.numeric(names(x[x == max(x)])) * max(x) #use this line if interested in sign of change with most agreement and proportion of agreement
    } else{
      0
    }
  }

  #apply the find.most.sign function to the array
  array1_most_sign = apply(array1_sign, c(2, 3), find.most.sign)

  return(array1_most_sign)

}


find.agreement = function(x, threshold) {
  #calculate proportion of models predicting each sign of change (negative(-1), no change(0), positive(+1))
  sign.proportion = c(length(x[x < 0]) / length(x),
                      length(x[x == 0]) / length(x),
                      length(x[x > 0]) / length(x))
  names(sign.proportion) = c(-1, 0, 1)
  #compare the set threshold to the maximum proportion of models agreeing on any one sign of change
  #if the max proportion is higher than threshold, return 1 (meaning there is agreement in signs among model)
  #otherwise return 0 (no agreement meeting the set threshold)
  if (max(sign.proportion) > threshold) {
    return(1)
  } else{
    return(0)
  }
}

#' Model agreement
#'
#' function to find models members sign agreement.
#'
#' @param array3d 3d array in which first dimension is model member, usually C4R$Data. This can be obtained as a result of
#' bindGrid(data, dimension = "member"). Temporal dimension needs to be removed through another function
#' @param threshold numeric (0.5-1). Percentage of model agreement required
#' @return array with 0 (no model agreement) or 1 (model agreement), based on threshold


agreement = function(array3d, threshold) {
  array1_agreement = apply(array3d, c(2, 3), find.agreement, threshold)
}

make_raster <- function(cl4.object) {
  if (length(dim(cl4.object$Data)) != 2)
    stop("Your data needs to be a 2d array, check dimension")

  rasters <- raster::raster(
    cl4.object$Data,
    xmn = min(cl4.object$xyCoords$x),
    xmx = max(cl4.object$xyCoords$x),
    ymn = min(cl4.object$xyCoords$y),
    ymx = max(cl4.object$xyCoords$y)
  ) %>%
    flip(., direction = 'y')

  nms <-
    paste0(
      str_extract(cl4.object$Dates$start[1], "\\d{4}"),
      "_",
      str_extract(cl4.object$Dates$end[length(cl4.object$Dates$start)],  "\\d{4}")
    )
  names(rasters) <-  nms

  raster::crs(rasters) <- sp::CRS("+init=epsg:4326")

  return(rasters)

}

#' Consecutive days
#'
#' Calculation of consecutive days. It can be used with aggregateGrid.
#'
#' @param col numeric vector
#' @param duration either "max" or "total".
#' @param lowert numeric. lower threshold
#' @param uppert numeric. upper threshold
#' @return numeric of length 1

# functions for consecutive days
thrs_consec = function(col, duration, lowert, uppert, ...) {

  if (!is.numeric(col))

    stop("input has to be a numeric vector")

  duration = match.arg(duration, choices = c("max", "total"))
  #analyse consecutive days

  if(!is.null(lowert)){

    consec = rle(col < lowert)

  } else{

    consec = rle(col > uppert)

  }

  #get only copnsecutive days matching the threshold

  consec_days = consec$lengths[consec$values==TRUE]

  #return values out

  if(duration == "max"){

    return(max(consec_days))

  } else{

    return(sum(consec_days[consec_days >= 6]))

  }

}


#' Calculation of thresholds
#'
#' Calculation of number of days with certain condition. It can be used with aggregateGrid.
#'
#' @param col numeric vector
#' @param lowert numeric. lower threshold
#' @param uppert numeric. upper threshold
#' @return numeric of length 1


thrs = function(col, lowert, uppert, ...) {

  if (!is.numeric(col))

    stop("input has to be a numeric vector")

  if(!is.null(lowert)){

    sum(precip_col < lowert)

  } else{

    sum(precip_col > uppert)

  }

}



# time of emergence. Yes when mean/SD > or < 1 for at least 5 consecutive years
ToE <- function(x, array)  {
  array_names <- dimnames(array)[3]
  if(is.null(array_names)) stop("Your array does not have named 3rd dimension. Check your input")
  array_names <- as.numeric(unlist(array_names))

  if (length(x)<5) stop("your data contains 6 time series only")

  tmp <- vector(mode="numeric", length=(length(x)-5))

  for (i in 1:(length(x)-5)) {


    if (x[i]>=1 & x[i+1]>=1  & x[i+2]>=1 & x[i+3]>=1 & x[i+4]>=1 & x[i+5]>=1) {

      tmp[i] <- array_names[i]

    } else if (x[i]<=-1 & x[i+1]<=-1  & x[i+2]<=-1  & x[i+3]<=-1  & x[i+4]<=-1  & x[i+5]<=-1 ) {

      tmp[i] <- -array_names[i]

    }  else
      tmp[i]  <- NA

  }

  if (any(is.numeric(tmp)))  {

    tmp <- tmp[!is.na(tmp)] [1]

  } else {

    tmp <- tmp[is.na(tmp)][1] }

  return(tmp)

}
