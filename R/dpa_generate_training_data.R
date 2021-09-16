dpa_generate_training_data = function(){
  
  dpa_classifier_init()
  
  
  # Get BIDs of (ir)relevant leads for training -----------------------------
  
  
  # the data is stored in the gtamain db in a way that would require unnecessary preprocessing
  
  source("setup/keys/gtamain.R")
  
  pool <<- pool::dbPool(
    drv = RMySQL::MySQL(),
    host = db.host,
    username = db.user,
    password = db.password,
    dbname=db.name
  )
  
  #not needed when used as a function
  #rm(db.host, db.user, db.password, db.name)
  
  #BIDs of leads that were removed and marked as useful
  sql.rlv = "SELECT gl.bastiat_id
FROM gta_leads gl
WHERE gl.id IN 
	#seen, marked relevant and removed, 
	#merged with either seen or not seen, 
	#and theme assigned 
	#(because assgining a theme means it will be rlv)
	(SELECT gl.id
	FROM gta_leads gl
	WHERE gl.display_id = 3
	AND gl.is_remove = 1
	AND gl.removal_reason = 13
	
	UNION
	
	SELECT gl.id
	FROM gta_leads gl
	JOIN gta_lead_theme glt
	ON gl.id = glt.lead_id
	JOIN gta_theme_list gtl
	ON glt.theme_id=gtl.theme_id
	WHERE gtl.display_dpa=1
	AND gl.is_remove=0)"
  
  #BIDs of removed leads that were removed for NOT being useful
  sql.irv = "SELECT gl.bastiat_id
FROM gta_leads gl
WHERE gl.is_remove = 1
AND gl.display_id = 3
AND gl.id NOT IN (
  SELECT gl.id
  FROM gta_leads gl
  WHERE gl.id IN 
  	#seen, marked relevant and removed, 
  	#merged with either seen or not seen, 
  	#and theme assigned 
  	#(because assgining a theme means it will be rlv)
  	(SELECT gl.id
  	FROM gta_leads gl
  	WHERE gl.display_id = 3
  	AND gl.is_remove = 1
  	AND gl.removal_reason = 13
  	
  	UNION
  	
  	SELECT gl.id
  	FROM gta_leads gl
  	JOIN gta_lead_theme glt
  	ON gl.id = glt.lead_id
  	JOIN gta_theme_list gtl
  	ON glt.theme_id=gtl.theme_id
  	WHERE gtl.display_dpa=1
  	AND gl.is_remove=0)
    )"
  
  bids.rlv = gta_sql_get_value(query = sql.rlv)
  bids.irv = gta_sql_get_value(query = sql.irv)
  
  poolClose(pool)
  rm(sql.irv, sql.rlv)
  
  
  
  
  # get training data from ricardodb ----------------------------------------
  
  
  print("Opening gtaricardo connection...")
  
  bastiat.wd = paste0(str_extract(getwd(), ".+Dropbox/"), "Bastiat")
  #setwd("/home/rstudio/Dropbox/Bastiat")
  setwd(bastiat.wd)
  source("setup/keys/ric.R")
  pool <<- pool::dbPool(
    drv = RMySQL::MySQL(),
    host = db.host,
    username = db.user,
    password = db.password,
    dbname=db.name
  )
  
  #not needed when used as a function
  #rm(db.host, db.user, db.password, db.name)
  
  # use relevant BIDs from above to build training data for relevant leads
  
  #prepare for SQLification
  bids.rlv = paste("'", bids.rlv, "'", sep = "", collapse = ", ")
  bids.irv = paste("'", bids.irv, "'", sep = "", collapse = ", ")
  
  sql.rlv = paste0("SELECT distinct btbid.hint_id, 
btbid.bid, bthl.acting_agency, btht.hint_title, 
btht.hint_description, bthl.hint_values, bthl.registration_date, 
gtajl.jurisdiction_name, bthl.hint_state_id
FROM bt_hint_log bthl,
	bt_hint_bid btbid,
	bt_hint_text btht,
	bt_hint_jurisdiction bthj,
	gta_jurisdiction_list gtajl

WHERE bthl.hint_id = btbid.hint_id

#cartesian corresp
AND bthl.hint_id = btht.hint_id
AND bthl.hint_id = bthj.hint_id

AND btbid.hint_id = bthj.hint_id
AND btbid.hint_id = btht.hint_id

AND bthj.hint_id = btht.hint_id

AND bthj.jurisdiction_id = gtajl.jurisdiction_id

#filters
AND btht.language_id = 1
AND bthl.registration_date > '2021-06-01'

AND bthl.hint_type_id = 3
AND btbid.bid IN (", bids.rlv, ");")
  
  
  #use irrelevant BIDs from above to build training corpus for irrelevant leads
  sql.irv = paste0("SELECT distinct btbid.hint_id, 
btbid.bid, bthl.acting_agency, btht.hint_title, 
btht.hint_description, bthl.hint_values, bthl.registration_date, 
gtajl.jurisdiction_name, bthl.hint_state_id
FROM bt_hint_log bthl,
	bt_hint_bid btbid,
	bt_hint_text btht,
	bt_hint_jurisdiction bthj,
	gta_jurisdiction_list gtajl

WHERE bthl.hint_id = btbid.hint_id

#cartesian corresp
AND bthl.hint_id = btht.hint_id
AND bthl.hint_id = bthj.hint_id

AND btbid.hint_id = bthj.hint_id
AND btbid.hint_id = btht.hint_id

AND bthj.hint_id = btht.hint_id

AND bthj.jurisdiction_id = gtajl.jurisdiction_id

#filters
AND btht.language_id = 1
AND bthl.registration_date > '2021-06-01'

AND bthl.hint_type_id = 3
AND btbid.bid IN (", bids.irv, ");")
  
  
  td.rlv = gta_sql_get_value(query = sql.rlv)
  td.irv = gta_sql_get_value(query = sql.irv)
  
  training.full = rbind(td.rlv, td.irv)
  
  training.full$relevant = training.full$bid %in% td.rlv$bid %>% as.numeric()
  
  #there are duplicates due to some initial scraping errors
  training.full = subset(training.full, !duplicated(training.full$hint.description))
  
  return(training.full)  
}  


