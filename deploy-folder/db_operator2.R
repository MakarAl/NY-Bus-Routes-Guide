library(RMySQL)

# function for loading data from DB
loadData <- function(queryInput){
    con <- dbConnect(RMySQL::MySQL(), 
                     dbname = "cs4111", 
                     host = "cs4111.ctvs3tjeewtz.us-west-2.rds.amazonaws.com", 
                     port = 3306, 
                     username = "****", #enter proper db credentials
                     password = "****") #enter proper db credentials
    data <- dbGetQuery(con, queryInput)
    dbDisconnect(con)
    data
}

# function for receiving stops list from db
loadStops <- function(){
    query <- "SELECT stop_id AS id, stop_lat AS lat,
                     stop_lon AS lon, stop_name AS nam
              FROM Stops"
    loadData(query)
}

# function for receiving all routes for dropdown select
loadAllRoutes <- function(){
    query <- "SELECT route_id
              FROM Routes"
    loadData(query)
}

# function for receiving info about selected route
loadRouteInfo <- function(route){
    query <- paste0("SELECT route_short_name, route_long_name, route_desc
              FROM Routes
              WHERE route_id = '",route,"'")
    loadData(query)
}

# function for receiving routes through selected stop
loadRoutes_ <- function(selectedStop,date){
    weekday <- weekdays(as.Date(date,'%Y-%m-%d'))
    work_days <-
       switch(weekday,
           Monday = "'1______'",
           Tuesday = "'_1_____'",
           Wednesday = "'__1____'",
           Thursday = "'___1___'",
           Friday = "'____1__'",
           Saturday = "'_____1_'",
           Sunday = "'______1'"
       )
  
    query <- paste(
  "SELECT DISTINCT route_id
   FROM Trips
   WHERE trip_id IN (SELECT T.trip_id
                     FROM Trips T, (SELECT C.service_id
                                    FROM Calendar C
                                    WHERE C.work_days LIKE ",work_days," AND start_date <= '",date,"' AND end_date >= '",date,"'
                                          AND C.service_id NOT IN (SELECT CD.service_id
                                                                   FROM Calendar_date CD
                                                                   WHERE CD.date = '",date,"' AND CD.exception_type = 2) 
                                    UNION
                                    SELECT CD.service_id
                                    FROM Calendar_date CD
                                    WHERE CD.date = '",date,"' AND CD.exception_type = 1)Service
                     WHERE T.trip_id IN (SELECT P.trip_id
                                         FROM Pass_by P
                                         WHERE P.stop_id IN (SELECT S.stop_id
                                         FROM Stops S
                                         WHERE S.stop_name = '",selectedStop,"')));", sep = "")
    loadData(query)
}

#####test####
hugetable <-function(selectedStop,date){
  weekday <- weekdays(as.Date(date,'%Y-%m-%d'))
  work_days <-
    switch(weekday,
           Monday = "'1______'",
           Tuesday = "'_1_____'",
           Wednesday = "'__1____'",
           Thursday = "'___1___'",
           Friday = "'____1__'",
           Saturday = "'_____1_'",
           Sunday = "'______1'"
    )
  
  query <- paste(
    "SELECT DISTINCT trip_id,stop_id,arrival_time
   FROM Pass_by
   WHERE trip_id IN (SELECT T.trip_id
                     FROM Trips T, (SELECT C.service_id
                                    FROM Calendar C
                                    WHERE C.work_days LIKE ",work_days," AND start_date <= '",date,"' AND end_date >= '",date,"'
                                          AND C.service_id NOT IN (SELECT CD.service_id
                                                                   FROM Calendar_date CD
                                                                   WHERE CD.date = '",date,"' AND CD.exception_type = 2) 
                                    UNION
                                    SELECT CD.service_id
                                    FROM Calendar_date CD
                                    WHERE CD.date = '",date,"' AND CD.exception_type = 1)Service
                     WHERE T.trip_id IN (SELECT P.trip_id
                                         FROM Pass_by P
                                         WHERE P.stop_id IN (SELECT S.stop_id
                                         FROM Stops S
                                         WHERE S.stop_name = '",selectedStop,"')));", sep = "")
  loadData(query)
}

loadRoutes <- function(selectedStop,date){
  weekday <- weekdays(as.Date(date,'%Y-%m-%d'))
  work_days <-
    switch(weekday,
           Monday = "'1______'",
           Tuesday = "'_1_____'",
           Wednesday = "'__1____'",
           Thursday = "'___1___'",
           Friday = "'____1__'",
           Saturday = "'_____1_'",
           Sunday = "'______1'"
    )
  
  query <- paste(
    "SELECT DISTINCT T1.route_id
   FROM Trips T1, Trips T2
   WHERE T1.trip_id IN (SELECT T.trip_id
                       FROM Trips T, (SELECT C.service_id
                                    FROM Calendar C
                                    WHERE C.work_days LIKE ",work_days," AND start_date <= '",date,"' AND end_date >= '",date,"'
                                          AND C.service_id NOT IN (SELECT CD.service_id
                                                                   FROM Calendar_date CD
                                                                   WHERE CD.date = '",date,"' AND CD.exception_type = 2) 
                                    UNION
                                    SELECT CD.service_id
                                    FROM Calendar_date CD
                                    WHERE CD.date = '",date,"' AND CD.exception_type = 1)Service)
          AND T2.trip_id IN (SELECT P.trip_id
                             FROM Pass_by P
                             WHERE P.stop_id IN (SELECT S.stop_id
                                                 FROM Stops S
                                                 WHERE S.stop_name = '",selectedStop,"'))
          AND T1.trip_id = T2.trip_id;", sep = "")
  loadData(query)
}


######

# aux function for retreiving proper shape for the route
Route2shape <- function(route_id,date){
  weekday <- weekdays(as.Date(date,'%Y-%m-%d'))
  work_days <-
    switch(weekday,
           Monday = "'1______'",
           Tuesday = "'_1_____'",
           Wednesday = "'__1____'",
           Thursday = "'___1___'",
           Friday = "'____1__'",
           Saturday = "'_____1_'",
           Sunday = "'______1'"
    )
  
  query <- paste(
    "SELECT *
     FROM Shapes S
     WHERE S.shape_id IN (SELECT tmp1.shape_id
                          FROM (SELECT tmp.shape_id, tmp.trip_no
                                FROM (SELECT Valid_trip.shape_id, COUNT(Valid_trip.trip_id) AS trip_no
                                      FROM (SELECT T.trip_id, T.shape_id
                                            FROM Trips T, (SELECT C.service_id
                                                           FROM Calendar C
                                                           WHERE C.work_days LIKE ",work_days," AND start_date <= '",date,"' AND end_date >= '",date,"'
                                                                 AND C.service_id NOT IN (SELECT CD.service_id
                                                                                          FROM Calendar_date CD
                                                                                          WHERE CD.date = '",date,"' AND CD.exception_type = 2) 
                                                           UNION
                                                           SELECT CD.service_id
                                                           FROM Calendar_date CD
                                                           WHERE CD.date = '",date,"' AND CD.exception_type = 1)Service
                                            WHERE T.route_id = '",route_id,"' AND T.service_id = Service.service_id)Valid_trip
                                     GROUP BY Valid_trip.shape_id)tmp
                          WHERE tmp.trip_no =  (SELECT MAX(tmp2.trip_no)
                                                FROM (SELECT Valid_trip.shape_id, COUNT(Valid_trip.trip_id) AS trip_no
                                                      FROM (SELECT T.trip_id, T.shape_id
                                                            FROM Trips T, (SELECT C.service_id
                                                                           FROM Calendar C
                                                                           WHERE C.work_days LIKE ",work_days," AND start_date <= '",date,"' AND end_date >= '",date,"'
                                                                                 AND C.service_id NOT IN (SELECT CD.service_id
                                                                                                          FROM Calendar_date CD
                                                                                                          WHERE CD.date = '",date,"' AND CD.exception_type = 2) 
                                                                           UNION
                                                                           SELECT CD.service_id
                                                                           FROM Calendar_date CD
                                                                           WHERE CD.date = '",date,"' AND CD.exception_type = 1)Service
                                                             WHERE T.route_id = '",route_id,"' AND T.service_id = Service.service_id)Valid_trip
                                                      GROUP BY Valid_trip.shape_id)tmp2))tmp1)
    ORDER BY shape_pt_seq;", sep = ""
  )
  query
}

# function for receiving shapes for selected route
loadShapes <- function(selectedRoute, selectedDate){
    query <- Route2shape(selectedRoute, selectedDate)
    loadData(query)
}

# function for receiving color for selected route
loadColor <- function(selectedRoute){
    query <- paste0(
            "SELECT route_color
            FROM Routes
            WHERE route_id = '",selectedRoute,"'"
    )
    loadData(query)
}

# function for receiving schedule
Schedule <- function(route_id,date,stop_name,selected_time){
  weekday <- weekdays(as.Date(date,'%Y-%m-%d'))
  work_days <-
    switch(weekday,
           Monday = "'1______'",
           Tuesday = "'_1_____'",
           Wednesday = "'__1____'",
           Thursday = "'___1___'",
           Friday = "'____1__'",
           Saturday = "'_____1_'",
           Sunday = "'______1'"
    )
  query <- paste(
    "SELECT arrival_time
     FROM Pass_by P, Stops S
     WHERE P.stop_id = S.stop_id AND S.stop_name = '",stop_name,"' 
           AND P.trip_id IN (SELECT T.trip_id
                             FROM Trips T, (SELECT C.service_id
                                            FROM Calendar C
                                            WHERE C.work_days LIKE ",work_days," AND start_date <= '",date,"' AND end_date >= '",date,"'
                                                  AND C.service_id NOT IN (SELECT CD.service_id
                                                                           FROM Calendar_date CD
                                                                           WHERE CD.date = '",date,"' AND CD.exception_type = 2) 
                                            UNION
                                            SELECT CD.service_id
                                            FROM Calendar_date CD
                                            WHERE CD.date = '",date,"' AND CD.exception_type = 1)Service
                            WHERE T.route_id = '",route_id,"' AND T.service_id = Service.service_id)
    ORDER BY P.arrival_time ASC;",sep = ""
  )

  temp_sche <- loadData(query)
  sep_sche <- matrix(nrow = nrow(temp_sche),ncol = 3)
  for(k in 1:nrow(temp_sche)){
    sep_sche[k,1] <- (unlist(strsplit(as.character(temp_sche[k,]),":")))[1]
    sep_sche[k,2] <- (unlist(strsplit(as.character(temp_sche[k,]),":")))[2]
    sep_sche[k,3] <- (unlist(strsplit(as.character(temp_sche[k,]),":")))[3]
  }
  
  sep_time <- unlist(strsplit(as.character(selected_time),":"))
  for(k in 1:nrow(sep_sche)){
   if(sep_sche[k,1] > sep_time[1]){
     break
   }else if(sep_sche[k,1] == sep_time[1]){
       if(sep_sche[k,2] > sep_time[2]){
         break         
       }else if((sep_sche[k,2] == sep_time[2])){
         if(sep_sche[k,3] >= sep_time[3]){
           break
         } 
       }       
    }
  }
  sche <- data.frame()
  for(j in 1:(nrow(temp_sche)-k+1)){
     sche[j,1] = temp_sche[k+j-1,]
  }
 sche
}
