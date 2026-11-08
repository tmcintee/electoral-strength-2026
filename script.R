require(tidyverse)
source("house_aggregate.R")
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
			total_votes,
			magnitude) %>%
		mutate(strength_composite = (2*votes - median(votes)- mean(votes) - 
				2*votes_opp + median(votes_opp) + mean(votes_opp))/
				sqrt(100*total_votes), # Arbitrary scalar factor of 10 to bring the observed score range to a more intuitive range.
			vote_vs_median = votes - median(votes),
			vote_vs_median_opp = votes_opp - median(votes_opp),
			vote_vs_mean = votes - mean(votes),
			vote_vs_mean_opp = votes_opp - mean(votes_opp),
			share_vs_median = vote_share - median(vote_share),
			share_vs_median_opp = vote_opp_share - median(vote_opp_share),
			share_vs_mean = vote_share - mean(vote_share),
			share_vs_mean_opp = vote_opp_share - mean(vote_opp_share)) %>%
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
		!is.na(votes),
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
	calculate_strength() %>%
	filter_interest()

within_office <- statewide_returns_most %>%
	group_by(state,office,party_std) %>%
	calculate_strength() %>%
	filter_interest()

within_state <- statewide_returns_most %>%
	group_by(state,party_std) %>%
	calculate_strength() %>%
	filter_interest()

presidential_benchmark <- statewide_returns_most %>%
	group_by(state,party_std) %>%
	filter((year %% 4 == 0 & office == "President" )|
			(year == 2026 & office %in% c("Senate","Governor"))) %>%
	calculate_strength() %>%
	filter_interest()


four_strength_measures <- within_ballot %>%
	#select(candidate, party_std, office, state, strength_composite) %>%
	rename(within_ballot = strength_composite) %>%
	left_join(presidential_benchmark %>%
			select(candidate,strength_composite) %>%
			rename(vs_presidential = strength_composite)) %>%
	left_join(within_office %>%
			select(candidate,strength_composite) %>%
			rename(within_office = strength_composite)) %>%
	left_join(within_state %>%
			select(candidate,strength_composite) %>%
			rename(within_state = strength_composite)) %>%
	mutate(strength = (within_ballot + vs_presidential + within_office + within_state)/(4 - (within_ballot == 0))) %>%
	arrange(-strength)

dems <- four_strength_measures %>%
	filter(party_std == "DEM") %>%
	mutate(normalized_dem = 100 + 15*(strength - mean(strength))/sd(strength))
reps <- four_strength_measures %>%
	filter(party_std == "REP") %>%
	mutate(normalized_rep = 100 + 15*(strength - mean(strength))/sd(strength))

dem_sen <- dems %>%
	filter(office == "Senate") %>%
	select(candidate,state,normalized_dem,strength,vs_presidential,within_ballot,within_office,within_state)
ggplot(dems, aes(color = office, x = strength, y = vs_presidential - within_office, label = state))+
	geom_label(alpha = 0.5)+
	scale_color_manual(values = c("darkgreen","brown"))

mi_check <- statewide_returns_most %>% filter(state == "Michigan" & year == 2026)

joined <- reps %>%
	select(state, office, candidate) %>%
	rename(Republican = candidate) %>%
	inner_join(dems) %>%
	rename(State = state,
		Office = office,
		Democrat = candidate,
		Democratic_Performance_Advantage = strength,
		Democratic_vote = votes,
		Republican_vote = votes_opp) %>%
	mutate(Democratic_Margin_of_Victory = vote_share - vote_opp_share) %>%
	select(State,
		Office,
		Democrat,
		Republican,
		Democratic_Performance_Advantage,
		Democratic_vote,
		Republican_vote,
		Democratic_Margin_of_Victory) %>%
	arrange(-Democratic_Performance_Advantage)
write_csv(joined,"outputs/summary_performance_2026_senate_governors.csv")
write_csv(presidential_benchmark,"outputs/2026_senate_governors_versus_presidential.csv")
write_csv(within_ballot,"outputs/2026_senate_governors_within_ballot.csv")
write_csv(within_office,"outputs/2026_senate_governors_within_office.csv")
write_csv(within_office,"outputs/2026_senate_governors_within_state.csv")