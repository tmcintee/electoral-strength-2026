# house_aggregate.R
# Builds a virtual statewide "race" (office "House Delegation") from a state's whole House delegation, for states/years where EVERY district
# on the November ballot has both a Democrat and a Republican with vote counts, and injects it into
# statewide_returns_tall.csv as three candidates: House_Dem_<ST>_<YEAR>, House_Rep_<ST>_<YEAR>, House_Oth_<ST>_<YEAR>.
# Re-running replaces all previously injected aggregate rows (states that no longer qualify drop out).

library(tidyverse)

# ---- settings --------------------------------------------------------------------------------------
house_path <- "house_elections.csv"
tall_path <- "statewide_returns_tall.csv"
out_path <- "statewide_returns_tall.csv" # overwrite; point elsewhere to compare first
agg_office <- "House Delegation"
min_districts <- 2 # 1-district states should be in the main dataset as individual races.

rcv_era <- tribble(~state_abbr, ~from, # ranked-choice House elections
	"ME", 2018,
	"AK", 2022)
stage_order <- c("general", "jungle", "rcv_round1", "rcv_final") # first-round count preferred on the Nov ballot

# ---- read -------------------------------------------------------------------------------------------
house <- read_csv(house_path, guess_max = 1e5, show_col_types = FALSE)
tall <- read_csv(tall_path, guess_max = 1e5, show_col_types = FALSE)

# ---- one set of rows per district: the November ballot, first-round count --------------------------
regular_elec <- house %>%
	filter(!special) %>%
	group_by(year, state_abbr) %>%
	filter(election_date == min(election_date)) %>% # drops later runoffs
	group_by(year, state_abbr, district) %>%
	filter(match(stage, stage_order) == min(match(stage, stage_order))) %>%
	ungroup()

# ---- which state-years qualify ----------------------------------------------------------------------
district_ok <- regular_elec %>%
	group_by(year, state_abbr, district) %>%
	summarise(has_dem = any(party_std == "DEM" & !is.na(votes)),
		has_rep = any(party_std == "REP" & !is.na(votes)),
		all_votes = all(!is.na(votes[!party_std %in% c("WRI", "SCAT")])),
		.groups = "drop") %>%
	mutate(ok = has_dem & has_rep & all_votes)

state_ok <- district_ok %>%
	group_by(year, state_abbr) %>%
	summarize(n_districts = n(),
		n_ok = sum(ok),
		no_dem = sum(!has_dem),
		no_rep = sum(!has_rep),
		missing_cnt = sum(!all_votes),
		.groups = "drop") %>%
	mutate(qualifies = n_ok == n_districts & n_districts >= min_districts)

message(sprintf("%d of %d multi-district state-years qualify",
	sum(state_ok$qualifies), sum(state_ok$n_districts >= min_districts)))
print(state_ok %>% filter(qualifies) %>% count(year, name = "states") )

# ---- aggregate --------------------------------------------------------------------------------------
agg <- regular_elec %>%
	semi_join(filter(state_ok, qualifies), by = c("year", "state_abbr")) %>%
	mutate(bloc = case_when(party_std == "DEM" ~ "Dem",
		party_std == "REP" ~ "Rep",
		TRUE ~ "Oth")) %>%
	group_by(year, state_abbr, state, election_date, bloc) %>%
	summarize(votes = sum(votes, na.rm = TRUE),
		status_any = if (any(status == "partial", na.rm = TRUE)) "partial" else "final",
		final_round = any(stage == "rcv_final"),
		as_of = NA,
		.groups = "drop") %>%
	left_join(select(state_ok, year, state_abbr, n_dist = n_districts), by = c("year", "state_abbr")) %>%
	left_join(rcv_era, by = "state_abbr") %>%
	mutate(rcv = !is.na(from) & year >= from) %>%
	group_by(year, state_abbr) %>%
	mutate(final_round = any(final_round)) %>%
	ungroup() %>%
	group_by(year, state_abbr) %>%
	mutate(total_votes = sum(votes),
		vote_share = votes / total_votes,
		votes_opp = map_dbl(seq_along(votes), ~ max(votes[-.x])), # best other bloc
		winner = votes == max(votes),
		status = if (any(status_any == "partial")) "partial" else "final") %>%
	ungroup() %>%
	transmute(year, 
		state_abbr, 
		state,
		office = agg_office,
		seat = paste("Districts 1 through", n_dist),
		special = FALSE,
		stage = "general",
		candidate = paste("House", bloc, state_abbr, year, sep = "_"),
		party_std = recode(bloc, Dem = "DEM", Rep = "REP", Oth = "OTH"),
		votes, votes_opp, total_votes, vote_share, winner,
		incumbent = NA,
		uncontested = FALSE,
		two_major = TRUE,
		dem_backed_independent = FALSE,
		partisan = TRUE,
		rcv,
		is_deciding = TRUE,
		magnitude = 1L,
		seats_merged = TRUE, # flags the virtual aggregate
		status,
		as_of,
		notes = paste0(sprintf("Virtual race from combining all %d House districts on the November ballot, as all races featured at least one each of Democrats and Republicans", n_dist),
			if_else(final_round, "; includes district(s) where only the final ranked-choice round is recorded",
				if_else(rcv, "; ranked-choice: first-round counts", ""))),
		party_raw = recode(bloc, Dem = "Democrats (combined)", Rep = "Republicans (combined)", Oth = "Others (combined)"),
		election_date = as.character(election_date),
		ballot_id = paste0(state_abbr, "_", election_date),
		contest_id = paste(year, state_abbr, "house_all_districts", sep = "_"),
		race_id = paste0(contest_id, "_general"),
		candidate_id = candidate,
		candidate_id_check = FALSE,
		office_raw = "U.S. House delegation, all districts combined",
		source = "house_elections.csv (aggregated by house_aggregate.R)")

# ---- inject: drop any previous aggregate rows, add the new ones ---------------------------------------
out <- tall %>%
	filter(office != agg_office | is.na(office)) %>%
	mutate(election_date = as.character(election_date)) %>%
	bind_rows(agg) %>%
	arrange(year, state_abbr) # stable: existing order kept within state-year

write_csv(out, out_path, na = "")
message(sprintf("wrote %s: %d aggregate rows for %d state-years", out_path, nrow(agg), n_distinct(agg$contest_id)))
