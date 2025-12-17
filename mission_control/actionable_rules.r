### Prepare Protect ### 
highest_level <- function(a,b,c,d,e,f){
    if( a ){ "A_on" } 
    else if ( b ) { "A_off" } 
    else if ( c ) { "B_on" } 
    else if ( d ) { "B_off" }
    else if ( e ) { "C_on"}
    else if ( f ) { "C_off"}
    else { "other"}
}

evidence_map <- list("A_on" = "FDA Approved", 
                     "A_off" = "Drug Repurposing", 
                     "B_on" = "Experimental/Other Guidelines", 
                     "B_off" = "Other",
                     "C_on" = "Other",
                     "C_off" = "Other")

small_map <- list("High tumor mutational burden" = "TMB High",
                  "High tumor mutational load" = "TML High",
                  "Microsatellite unstable" = "MSI High")
small_names <- function( i ){
    if(i %in% names(small_map)){ small_map[[i]]}
    else { i }
}


### Formatting Rest for Oncoplot ### 
level_map <- list("FDA Approved" = "Rest FDA",
                  "Drug Repurposing" = "Rest Repurpose",
                  "Experimental/Other Guidelines" = "Rest Experimental",
                  "Other" = "Rest Other",
                  "None" = "None")

rank_update <- function( event_summary, k, rk){
    if( event_summary == "Rest FDA" ){ k + 1000 }
    else if ( event_summary == "Rest Repurpose" ){ k + 1001 }
    else if ( event_summary == "Rest Experimental"){ k + 1002 }
    else if ( event_summary == "Rest Other"){ k + 1003 }
    else if ( grepl("None", event_summary)){ k + 1004 }
    else { rk }    
}

### Events Name Mapping ### 
name_map <-list("FDA Approved" = "FDA",
                "Drug Repurposing" = "Off",
                "Experimental/Other Guidelines" = "Exp",
                "Other" = "Oth",
                "None" = "None")

highest_level_event <- function(a,b,c,d){
    if( a > 0 ){ "FDA"}
    else if ( b > 0 ) { "Off"}
    else if ( c > 0) { "Exp" }
    else if ( d > 0) { "Oth" }
    else { "None"}
}


