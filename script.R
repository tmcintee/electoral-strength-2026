require(tidyverse)
calculate_strength <- function(df_grouped)
{
	df_processed <- df_grouped %>%
		select(candidate,
			office,
			year,
			state,
			ballot_id,
			party_std,
			votes,
			votes_opp,
			vote_share,
			vote_opp_share,
			total_votes) %>%
		mutate(strength_composite = (2*votes - median(votes)- mean(votes) - 
				2*votes_opp + median(votes_opp) + mean(votes_opp))/
				sqrt(total_votes),
			vote_vs_median = votes - median(votes),
			vote_vs_median_opp = votes_opp - median(votes_opp),
			vote_vs_mean = votes - mean(votes),
			vote_vs_mean_opp = votes_opp - mean(votes_opp),
			share_vs_median = vote_share - median(vote_share),
			share_vs_median_opp = vote_opp_share - median(vote_opp_share),
			vote_vs_mean = vote_share - mean(vote_share),
			vote_vs_mean_opp = vote_opp_share - mean(vote_opp_share)) %>%
		arrange(strength_composite) %>%
		ungroup()
	return(df_processed)
}
filter_interest <- function(df_output)
{
	df_filtered <- df_output %>%
		filter(office %in% c("Senate","Governor"),
			party_std %in% c("DEM","REP"),
			year == 2026)
	return(df_filtered)
}
statewide_returns_tall <- read_csv("statewide_returns_tall.csv")

statewide_returns_tall <- statewide_returns_tall %>%
	mutate(vote_opp_share = votes_opp / total_votes)
	
ballots_of_interest <- statewide_returns_tall %>%
	filter(year == 2026,
		office %in% c("Senate","Governor"),
		is_deciding,
		partisan,
		two_major) %>%
	select(ballot_id) %>%
	distinct() %>%
	pull(ballot_id)

statewide_returns_most <- statewide_returns_tall %>%
	filter(partisan,
		two_major,
		vote_share > 0.1)

midterm_returns <- statewide_returns_most %>%
	filter(year %% 4 == 2)

within_ballot <- statewide_returns_most %>%
	filter(ballot_id %in% ballots_of_interest,
		!is.na(votes)) %>%
	group_by(ballot_id,party_std) %>%
	calculate_strength() %>%
	filter_interest()

midterm_benchmark <- midterm_returns %>%
	group_by(state,office,party_std) %>%
	filter(!is.na(votes)) %>%
	calculate_strength() %>%
	filter_interest()

presidential_benchmark <- statewide_returns_most %>%
	group_by(state,party_std) %>%
	filter((year %% 4 == 0 & office == "President" )|
			(year == 2026 & office %in% c("Senate","Governor"))) %>%
	calculate_strength() %>%
	filter_interest()


three_strength_measures <- within_ballot %>%
	select(candidate, party_std, office, state, strength_composite) %>%
	rename(within_ballot_strength = strength_composite) %>%
	inner_join(presidential_benchmark %>%
			select(candidate,strength_composite) %>%
			rename(presidential_strength = strength_composite)) %>%
	inner_join(midterm_benchmark %>%
			select(candidate,strength_composite) %>%
			rename(midterm_strength = strength_composite)) %>%
	mutate(compounded_strength_measure = within_ballot_strength + presidential_strength + midterm_strength) %>%
	arrange(-compounded_strength_measure)

dems <- three_strength_measures %>%
	filter(party_std == "DEM")
reps <- three_strength_measures %>%
	filter(party_std == "REP")

dem_sen <- dems %>%
	filter(office == "Senate")