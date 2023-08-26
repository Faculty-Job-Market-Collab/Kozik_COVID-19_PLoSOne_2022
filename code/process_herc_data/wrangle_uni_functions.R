fix_inst_name <- function(x){
  
  if(str_detect(x, "College|University") == FALSE){
    
    case_when(
      str_detect(x, "Tennessee State$|Bridgewater State$|North Carolina State$|Michigan State$|Kennesaw State$|Sam Houston State$|Georgia State$|Wichita State$|Auburn$|Arkansas Tech$|Colgate$|Vanderbilt$") ~ paste0(x, " University"),
      str_detect(x, "Springfield$|Dalton State$|Occidental$|Kentucky Wesleyan$") ~ paste0(x, " College"),
      str_detect(x, "State Billings") ~ "Montana State University Billings",
      str_detect(x, mult_loc_uni_last_list) ~ str_replace(x, ",", " University"),
      str_detect(x, mult_loc_uni_of_list) ~ paste0("University Of", x),
      str_detect(x, mult_loc_coll_of_list) ~ paste0("College Of", x),
      str_detect(x, mult_loc_college_last_list) ~ str_replace(x, ",", " College"),
      str_detect(x, x_college_list) ~ paste0(x, " College"),
      str_detect(x, x_uni_list) ~ paste0(x, " University"),
      TRUE ~ x
    )}else{
      x
    }
}

fix_inst_abbrv <- function(x){
  case_when(
    str_detect(x, "Ucsd") ~ "University Of California San Diego",
    str_detect(x, "Ucla") ~ "University Of California Los Angeles",
    str_detect(x, "Ucsf") ~ "University of California San Francisco",
    str_detect(x, "Ucsb") ~ "University Of California Santa Barbara",
    str_detect(x, "Uiuc") ~ "University Of Illinois Urbana Champaign",
    str_detect(x, "Uab") ~ "University Of Alabama Birmingham",
    str_detect(x, "Umaine") ~ "University Of Maine",
    str_detect(x, "Buffalo State") ~ "Suny Buffalo State",
    str_detect(x, "Suny Upstate") ~ str_remove(x, "Suny "),
    str_detect(x, "Cuny Graduate Center") ~ "Cuny Graduate School And University Center",
    str_detect(x, "Rose Hulman") ~ "Rose Hulman Institute of Technology",
    str_detect(x, "Uthscsa") ~ "University Of Texas Health Science Center San Antonio",
    str_detect(x, "Uthsc") ~ "University of Tennessee Health Science Center",
    str_detect(x, "University.*(?=Albany)") ~ "SUNY Albany",
    str_detect(x, "Uofsc") ~ "University Of South Carolina",
    str_detect(x, "Smu") ~ "Southern Methodist University",
    str_detect(x, "William And Mary") ~ "College Of William And Mary",
    str_detect(x, "Rutgers Medical School") ~ "Rutgers University Newark",
    str_detect(x, "Utsw|Ut (?=South)|Texas Southwestern$") ~ "University Of Texas Southwestern Medical Center",
    str_detect(x, "Md Anderson") ~ "The University Of Texas Md Anderson Cancer Center",
    str_detect(x, "Unc ") ~ str_replace(x,"Unc,?", "University Of North Carolina "),
    str_detect(x, "Cal State|Csu ") ~ str_replace(x, "Cal State|Csu", "California State University"),
    str_detect(x, "Umbc") ~ "University Of Maryland Baltimore County",
    str_detect(x, "Umn" ) ~ "University Of Minnesota",
    str_detect(x, "Mit" ) ~ "Massachusetts Institute Of Technology",
    str_detect(x, "Vcu" ) ~ "Virginia Commonwealth University",
    str_detect(x, "Utep") ~ "University Of Texas El Paso",
    str_detect(x, "Ut (?=Dallas)|Ut (?=Austin)") ~ str_replace(x,"Ut", "University Of Texas"),
    str_detect(x, "Cc") ~  str_replace(x, "Cc", "Community College"),
    str_detect(x, "Nc\\s?") ~ str_replace(x,"Nc","North Carolina"),
    str_detect(x, "Nw") ~  str_replace(x,"Nw", "Northwest"),
    str_detect(x, "Nyu") ~ "New York University",
    str_detect(x, "Uw")  ~ str_replace(x,"Uw", "University Of Wisconsin"),
    str_detect(x, "Uc (?=Denver)|Uc (?=Boulder)") ~ str_replace(x,"Uc", "University Of Colorado"),
    str_detect(x, "Um (?=Ann Arbor)|Um (?=Dearborn)|Um (?=Flint)") ~ str_replace(x, "Um", "University Of Michigan"),
    str_detect(x, "Uchic") ~ "University of Chicago",
    str_detect(x, "Umich") ~ "University Of Michigan Ann Arbor",
    str_detect(x, "Uc ")  ~ str_replace(x,"Uc", "University Of California"),
    str_detect(x, "Unh") ~ "University Of New Hampshire",
    str_detect(x, "St Bonaventure") ~ "St Bonaventure University",
    str_detect(x, "Penn |Penn$") ~ str_replace(x, "Penn |Penn$", "Pennsylvania "),
    str_detect(x, "Uva") ~ "University Of Virginia",
    str_detect(x, "Uvm") ~ "University Of Vermont",
    str_detect(x, "Tulane") ~ "Tulane University Of Louisiana",
    str_detect(x, "Lsu|Louisiana State University$") ~ "Louisiana State University And Agricultural And Mechanical College",
    str_detect(x, "Umass") ~ str_replace(x, "Umass", "University Of Massachusetts"),
    str_detect(x, "Ky$") ~ str_replace(x, "Ky$", "Kentucky"),
    str_detect(x, "Wi ") ~ str_replace(x, "Wi ", "Wisconsin "),
    str_detect(x, "Mn|Minn ") ~ str_replace(x, "Mn|Minn", "Minnesota"),
    str_detect(x, "Mich ") ~ str_replace(x, "Mich ", "Michigan "),
    str_detect(x, "Cal Poly") ~ str_replace(x, "Cal Poly", "California State Polytechnic University"),
    str_detect(x, "Dc") ~ str_replace(x, "Dc", "District Of Columbia"),
    str_detect(x, "Albany Med$") ~ "Albany Medical College",
    str_detect(x, "Wash University Stl") ~ "Washington University St Louis",
    str_detect(x, "Carolina Ch") ~ str_replace(x, "Ch$", "Chapel Hill"),
    str_detect(x, "Virginia Tech|Virginia Polytechnic And") ~ "Virginia Polytechnic Institute And State University",
    str_detect(x, "Wake Forest") ~ "Wake Forest University", 
    str_detect(x, "Harvard") ~ "Harvard University",
    str_detect(x, "Mount Sinai") ~ "Icahn School Of Medicine Mount Sinai",
    str_detect(x, "Duke") ~ "Duke University",
    str_detect(x, "Johns Hopkins|Hopkins$") ~ "Johns Hopkins University",
    str_detect(x, "Caltech") ~ "California Institute Of Technology",
    str_detect(x, "Mayo") ~ "Mayo Clinic College Of Medicine And Science",
    str_detect(x, "Weill Cornell") ~ "Weill Cornell Medical College",
    str_detect(x, "Columbia U|Columbia University$") ~ "Columbia University City Of New York",
    str_detect(x, "Yale") ~ "Yale University",
    str_detect(x, "Stanford") ~ "Stanford University",
    str_detect(x, "A And T") ~ paste0(x, " State University"),
    str_detect(x, "Aandm|AAndM") ~ str_replace(x, "Aandm|AAndM", "A And M"),
    str_detect(x, "Aandt|AAndT") ~ str_replace(x, "Aandt|AAndT", "A And T"),
    str_detect(x, "Wayne") ~ "Wayne State University",
    str_detect(x, "Of Texas Med") ~ "University Of Texas Medical Branch",
    str_detect(x, "St Mary") ~ "St Mary's College Of California",
    str_detect(x, "Of Massachusetts Medical") ~ paste0(x, " Worcester"),
    str_detect(x, "Merced") ~ "University Of California Merced",
    str_detect(x, "Western Michigan.*Medicine$") ~ "Western Michigan University Homer Stryker Md School Of Medicine",
    str_detect(x, "Of Rochester|Georgetown") ~ str_remove(x, " Medical Center"),
    str_detect(x, "West Virginia") ~ str_replace(x, "School", "Institute"),
    str_detect(x, "Champaign Urbana") ~ str_replace(x, "Champaign Urbana", "Urbana Champaign"),
    str_detect(x, "California Institute Of Teaching") ~ "California State University Chico",
    TRUE ~ x
  )
}

fix_campus <- function(x){
  case_when(
    str_detect(x, "Arizona State University$|Arizona State$") ~ "Arizona State University Tempe",
    str_detect(x, "Pennsylvania State University$|Pennsylvania State$|University Park$") ~ "Pennsylvania State University Main",
    str_detect(x, "Indiana University$") ~ "Indiana University Bloomington",
    str_detect(x, "Miami University") ~ "Miami University Oxford",
    str_detect(x, "Uni.* South Carolina$") ~ "University Of South Carolina Columbia",
    str_detect(x, "Uni.* (Of)? Michigan$") ~ "University Of Michigan Ann Arbor",
    str_detect(x, "Uni.* Wisconsin$") ~ "University of Wisconsin Madison",
    str_detect(x, "Midwestern University$") ~ "Midwestern University Downers Grove",
    str_detect(x, "Uni.* Minnesota$|Minnesota Minneapolis$") ~ "University Of Minnesota Twin Cities",
    str_detect(x, "Uni.* Hawaii$|Uni.* Hawai'i$") ~ "University Of Hawaii Manoa",
    str_detect(x, "Brigham Young University$") ~ "Brigham Young University Provo",
    str_detect(x, "Ohio State University$|Ohio State$") ~ "Ohio State University Main",
    str_detect(x, "Uni.* Cincinnati$") ~ "University Of Cincinnati Main",
    str_detect(x, "Rutgers Newark") ~ "Rutgers University Newark",
    str_detect(x, "Rutgers$|Rutgers University.?") ~ "Rutgers University New Brunswick",
    str_detect(x, "Pennsylvania State Uni.* Behrend|Pennsylvania State Behrend") ~ "Pennsylvania State University Erie Behrend College",
    str_detect(x, "Pennsylvania State Scranton") ~ "Pennsylvania State University Worthington Scranton",
    str_detect(x, "Northwest Missouri") ~ "Northwest Missouri State University",
    str_detect(x, "Uni.* Of Missouri") ~ "University Of Missouri Columbia",
    str_detect(x, "Missouri State Uni.*$") ~ "Missouri State University Springfield",
    str_detect(x, "Uni.* Of Montana Missoula") ~ str_remove(x, "Missoula"),
    str_detect(x, "Dartmouth|Dartmouth School Of Medicine") ~ "Dartmouth College",
    str_detect(x, "Uni.* Of Maryland Medical School") ~ "University Of Maryland Baltimore",
    str_detect(x, "Uni.* Maryland$") ~ "University Of Maryland College Park",
    str_detect(x, "Hostos|Queens College") ~ paste0("Cuny ", x),
    str_detect(x, "Stony Brook") ~ "Stony Brook University",
    str_detect(x, "Georgia I|Georgia Te") ~ "Georgia Institute Of Technology Main",
    str_detect(x, "Akron$") ~ "University Of Akron Main",
    str_detect(x, "North Carolina State University") ~ "North Carolina State University Raleigh",
    str_detect(x, "Of North Carolina$") ~ "University Of North Carolina Chapel Hill",
    str_detect(x, "Of Tennessee$") ~ "University Of Tennessee Knoxville",
    str_detect(x, "Colorado Anschutz|Colorado Denver|Colorado$|Colorado Medical") ~ "University Of Colorado Denver Anschutz Medical",
    str_detect(x, "Purdue$|Purdue University$") ~ "Purdue University Main",
    str_detect(x, "(?<!North )Texas$") ~ "University Of Texas Austin",
    str_detect(x, "(?<!Central )Oklahoma$") ~ "University Of Oklahoma Norman",
    str_detect(x, "Pittsburgh") ~ "University Of Pittsburgh Pittsburgh",
    str_detect(x, "South Florida") ~ "University Of South Florida Main",
    str_detect(x, "Washington State") ~ "Washington State University",
    str_detect(x, "Colorado State") ~ "Colorado State University Fort Collins",
    str_detect(x, "Washington University") ~ "Washington University St Louis",
    str_detect(x, "Of Washington") ~ "University Of Washington Seattle",
    str_detect(x, "Case Western") ~ "Case Western Reserve University", 
    str_detect(x, "Hampshire") ~ "University of New Hampshire Main",
    str_detect(x, "New Mexico (?=State)|Of New Mexico|Arkansas State|North Dakota|Of Virginia|Bowling Green|Wright State University$|Oklahoma State University$|Ohio University$") ~ paste(x, " Main"),
    str_detect(x, "Southern Ill") ~ "Southern Illinois University Carbondale",
    str_detect(x, "Embry") ~ "Embry Riddle Aeronautical University Daytona Beach",
    str_detect(x, "Texas A And M$|Texas A And M University$") ~ "Texas A And M University College Station",
    str_detect(x, "Of Nebraska$") ~ paste(x, " Lincoln"),
    str_detect(x, "Health Shreveport") ~ "Louisiana State University Health Sciences Center Shreveport",
    str_detect(x, "Kent State") ~ "Kent State University Kent",
    str_detect(x, "Mcgovern") ~ "University Of Texas Health Science Center Houston",
    str_detect(x, "Emmanuel|Ithaca") ~ str_remove(x, " Boston| Ithaca"),
    str_detect(x, "Nevada Reno") ~ "University of Nevada Reno",
    str_detect(x, "Central Lakes C") ~ "Central Lakes College Brainerd",
    str_detect(x, "University Of Illinois$") ~ "University Of Illinois Urbana Champaign",
    str_detect(x, "University Of Massachusetts$") ~ "University Of Massachusetts Amherst",
    TRUE ~ x
  )
}


#clean up university names
replace_uny <- function(x){
  
  x <- x
  
  if(str_detect(x, "Cuny|Suny|City University Of New York|State University Of New York") == TRUE){
    
    which_uny <- if_else(str_detect(str_to_title(x), "Cuny|City Of New"), "CUNY", "SUNY")
    
    if(which_uny == "CUNY"){
      no_cuny <- str_remove_all(x, " \\(Cuny\\)|CUNY$|Cuny$|^Cuny|Cuny|City University of New York")
      replace <- case_when(
        str_detect(x, "Graduate Center") ~ "Cuny Graduate School And University Center",
        str_detect(x, "Advanced Science Research Center") ~ "Cuny Graduate School And University Center",
        TRUE ~ paste0("Cuny", no_cuny)
      )
    }else{
      move <- str_replace(x, "(?<=Suny) .+ At|(?<=Suny) At|State University Of New York", "Suny")
      replace <- case_when(
        str_detect(move, "(?<!University) Buffalo$|(?<!Of) Buffalo$") ~ str_replace(move, "(?<!University)  Buffalo$", "Buffalo State"),
        str_detect(move, "Brockport") ~ "Suny College Brockport",
        str_detect(x, "Suny Upstate") ~ "Upstate Medical University",
        TRUE ~ move
      )
    }
    
    return(replace)
    
  }else{return(x)}
  
}