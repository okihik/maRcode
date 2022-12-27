isValidSheet <- function(sheet = sheet){
  isValid <- TRUE
  # Check for existence of sheet
  if(!exists("sheet")){
    isValid <- !isValid
    print("The object sheet does not exist.")
  }
  
  # Check if isClose is 0 or 1
  if(!is.logical(sheet$isClose)){
    isValid <- !isValid
    print("The isClose col is not logical.")
  }
  
  # Check for Date
  if(!is.Date(sheet$date)){
    isValid <- !isValid
    print("The date retrieved sheet is not Date format.")
  }
  
  # Check for negative values for measurements or observations
  if(!(all((sheet$fl|sheet$slfw|sheet$lfw|
            sheet$customers|sheet$orders|sheet$miniOrders|
            sheet$liquors|sheet$sales|sheet$takeouts >= 0) == TRUE))){
    isValid <- !isValid
    print("The date retrieved sheet has negative values.")
  }
  
  # Check if weather conditions (temperature) is between -50 to 50
  if(!(all(between(sheet$tMax|sheet$tMin|sheet$hrTM|sheet$mmTM,-50,50)))){
    isValid <- !isValid
    print("The date retrieved sheet has negative values.")
  }
  
  # Check if weather conditions (humiditiy) is between 0 to 100
  if(!(all(between(sheet$humMax|sheet$humMin|sheet$hrHM|sheet$mmHM,0,100)))){
    isValid <- !isValid
    print("The humidity date is not percentage.")
  }
  
  # Check if precipitation has negative value
  if(!(all((sheet$precip >= 0) == TRUE))){
    isValid <- !isValid
    print("The precipitation date has negative values.")
  }
  return(isValid)
}