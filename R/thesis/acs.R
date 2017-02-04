# ACS API 4accf218943e717796ac4e22b39d233f22befa03

library(acs)

api.key.install("4accf218943e717796ac4e22b39d233f22befa03")

allstates <- geo.make(state=state.name)

# get the table
race <- acs.lookup(table.name = "Race")
acs.lookup(table.number = "B02001")

# pull the race vars
(allstates_races <- acs.fetch(geography = allstates, variable = "B02001_001"))
                                                                

# fugsin with the 9.4M row data





