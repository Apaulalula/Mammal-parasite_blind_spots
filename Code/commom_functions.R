# Functions

remove_after_second_underscore <- function(x) sub("^(([^_]*_){1}[^_]*).*", "\\1", x)

taxonomic_distance <- function(sp1, sp2){
  
  if(sp1$HostSpecies_SBMz == sp2$HostSpecies_SBMz){
    return(0)
  }
  
  if(sp1$HostGenus == sp2$HostGenus){
    return(1)
  }
  
  if(sp1$HostFamily == sp2$HostFamily){
    return(2)
  }
  
  if(sp1$HostOrder == sp2$HostOrder){
    return(3)
  }
  
  return(4)
}
