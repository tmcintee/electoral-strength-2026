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
				sqrt(total_votes), # Arbitrary scalar factor of 10 to bring the observed score range to a more intuitive range.
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

macro_analysis_senate <- statewide_returns_tall %>%
	filter(ballot_id %in% ballots_of_interest, 
		party_std == "REP", 
		office == "Senate",
		!is.na(votes)) %>%
	left_join(statewide_returns_tall %>%
			filter(year == 2024 & office == "President" & party_std == "REP") %>%
			select(state, votes, votes_opp, total_votes) %>%
			rename(presidential_votes = votes,
				presidential_opp = votes_opp,
				presidential_total = total_votes)) %>%
	select(state,votes,votes_opp,total_votes, presidential_votes,presidential_opp,presidential_total) %>%
	mutate(margin = votes - votes_opp,
		presidential_margin = presidential_votes - presidential_opp)

sen_summary <- macro_analysis_senate %>%
	summarize(votes = sum(votes),
		votes_opp = sum(votes_opp),
		total_votes = sum(total_votes),
		presidential_votes = sum(presidential_votes),
		presidential_opp = sum(presidential_opp),
		presidential_total = sum(presidential_total),
		margin = sum(margin),
		presidential_margin = sum(presidential_margin)) %>%
	mutate(margin_pct = margin/total_votes,
		pres_margin_pct = presidential_margin/presidential_total) %>%
	mutate(delta = margin_pct - pres_margin_pct)


macro_analysis_gov <- statewide_returns_tall %>%
	filter(ballot_id %in% ballots_of_interest, 
		party_std == "REP", 
		office == "Governor",
		!is.na(votes)) %>%
	left_join(statewide_returns_tall %>%
			filter(year == 2024 & office == "President" & party_std == "REP") %>%
			select(state, votes, votes_opp, total_votes) %>%
			rename(presidential_votes = votes,
				presidential_opp = votes_opp,
				presidential_total = total_votes)) %>%
	select(state,votes,votes_opp,total_votes, presidential_votes,presidential_opp,presidential_total) %>%
	mutate(margin = votes - votes_opp,
		presidential_margin = presidential_votes - presidential_opp)

gov_summary <- macro_analysis_gov %>%
	summarize(votes = sum(votes),
		votes_opp = sum(votes_opp),
		total_votes = sum(total_votes),
		presidential_votes = sum(presidential_votes),
		presidential_opp = sum(presidential_opp),
		presidential_total = sum(presidential_total),
		margin = sum(margin),
		presidential_margin = sum(presidential_margin)) %>%
	mutate(margin_pct = margin/total_votes,
		pres_margin_pct = presidential_margin/presidential_total) %>%
	mutate(delta = margin_pct - pres_margin_pct)


statewide_returns_most <- statewide_returns_tall %>%
	filter(partisan,
		two_major,
		!strong_third,
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
	mutate(within_ballot = within_ballot * 100/max(within_ballot),
		vs_presidential = vs_presidential * 100/max(vs_presidential),
		within_office = within_office * 100/max(within_office),
		within_state = within_state * 100/max(within_state)) %>%
	mutate(strength = (within_ballot + vs_presidential + within_office + within_state)/(4 - (within_ballot == 0))) %>%
	arrange(-strength)

dems <- four_strength_measures %>%
	filter(party_std == "DEM") %>%
	select(candidate,
		state,
		office,
		strength,
		vs_presidential,
		within_ballot,
		within_office,
		within_state,
		votes,
		votes_opp,
		total_votes,
		vote_share,
		vote_opp_share) %>%
	mutate(normalized_dem = 100 + 15*(strength - mean(strength))/sd(strength))
reps <- four_strength_measures %>%
	filter(party_std == "REP") %>%
	select(candidate,
		state,
		office,
		strength,
		vs_presidential,
		within_ballot,
		within_office,
		within_state,
		votes,
		votes_opp,
		total_votes,
		vote_share,
		vote_opp_share) %>%
	mutate(normalized_rep = 100 + 15*(strength - mean(strength))/sd(strength))

dem_sen <- dems %>%
	filter(office == "Senate") 
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
#write_csv(joined,"outputs/summary_performance_2026_senate_governors.csv")
#write_csv(presidential_benchmark,"outputs/2026_senate_governors_versus_presidential.csv")
#write_csv(within_ballot,"outputs/2026_senate_governors_within_ballot.csv")
#write_csv(within_office,"outputs/2026_senate_governors_within_office.csv")
#write_csv(within_office,"outputs/2026_senate_governors_within_state.csv")

# Idea: Do an arrow plot showing raw shifts from Harris 2024, within-ballot, within-state, and within-office
# Add "short name" field? Or use state TLA? Probably 2-letter codes easiest.
presidential_votes <- statewide_returns_most %>%
	filter(office == "President" & party_std == "REP") %>%
	select(state,vote_share,vote_opp_share) %>%
	group_by(state) %>%
	summarize(presidential_R_mean = mean(vote_share),
		presidential_D_mean = mean(vote_opp_share),
		presidential_R_median = median(vote_share),
		presidential_D_median = median(vote_opp_share))

past_governor_votes <- statewide_returns_most %>%
	filter(year != 2026,office == "Governor",party_std == "REP") %>%
	select(state,vote_share,vote_opp_share) %>%
	group_by(state) %>%
	summarize(within_office_R_mean = mean(vote_share),
		within_office_D_mean = mean(vote_opp_share),
		within_office_R_median = median(vote_share),
		within_office_D_median = median(vote_opp_share)) %>%
	mutate(office = "Governor")

past_senator_votes <- statewide_returns_most %>%
	filter(year != 2026,office == "Senate",party_std == "REP") %>%
	select(state,vote_share,vote_opp_share) %>%
	group_by(state) %>%
	summarize(within_office_R_mean = mean(vote_share),
		within_office_D_mean = mean(vote_opp_share),
		within_office_R_median = median(vote_share),
		within_office_D_median = median(vote_opp_share)) %>%
	mutate(office = "Senate")

within_ballot_sen <- statewide_returns_most %>%
	filter(year == 2026,office != "Senate",party_std == "REP") %>%
	select(state,vote_share,vote_opp_share) %>%
	group_by(state) %>%
	summarize(within_ballot_R_mean = mean(vote_share),
		within_ballot_D_mean = mean(vote_opp_share),
		within_ballot_R_median = median(vote_share),
		within_ballot_D_median = median(vote_opp_share)) %>%
	mutate(office = "Senate")

within_ballot_gov <- statewide_returns_most %>%
	filter(year == 2026,office != "Governor",party_std == "REP") %>%
	select(state,vote_share,vote_opp_share) %>%
	group_by(state) %>%
	summarize(within_ballot_R_mean = mean(vote_share),
		within_ballot_D_mean = mean(vote_opp_share),
		within_ballot_R_median = median(vote_share),
		within_ballot_D_median = median(vote_opp_share)) %>%
	mutate(office = "Governor")

within_state_sen <- statewide_returns_most %>%
	filter(!(year == 2026 & office == "Senate"),party_std == "REP") %>%
	select(state,vote_share,vote_opp_share) %>%
	group_by(state) %>%
	summarize(within_state_R_mean = mean(vote_share),
		within_state_D_mean = mean(vote_opp_share),
		within_state_R_median = median(vote_share),
		within_state_D_median = median(vote_opp_share)) %>%
	mutate(office = "Senate")

within_state_gov <- statewide_returns_most %>%
	filter(!(year == 2026 & office == "Governor"),party_std == "REP") %>%
	select(state,vote_share,vote_opp_share) %>%
	group_by(state) %>%
	summarize(within_state_R_mean = mean(vote_share),
		within_state_D_mean = mean(vote_opp_share),
		within_state_R_median = median(vote_share),
		within_state_D_median = median(vote_opp_share)) %>%
	mutate(office = "Governor")

within_state <- bind_rows(within_state_sen,within_state_gov)
within_ballot <- bind_rows(within_ballot_sen,within_ballot_gov)
within_office <- bind_rows(past_senator_votes,past_governor_votes)

comparison <- statewide_returns_most %>%
	filter(year == 2026,office %in% c("Governor","Senate"),party_std == "REP",state != "Alaska") %>%
	select(office,state,vote_share,vote_opp_share) %>%
	rename(votes_R = vote_share,
		votes_D = vote_opp_share) %>%
	left_join(presidential_votes) %>%
	left_join(within_state) %>%
	left_join(within_office) %>%
	left_join(within_ballot) %>%
	mutate(delta_presidential_R = votes_R - presidential_R_mean,
		delta_presidential_D = votes_D - presidential_D_mean,
		delta_ballot_R = votes_R - within_ballot_R_mean,
		delta_ballot_D = votes_D - within_ballot_D_mean,
		delta_office_R = votes_R - within_office_R_mean,
		delta_office_D = votes_D - within_office_D_mean,
		delta_state_R = votes_R - within_state_R_mean,
		delta_state_D = votes_D - within_state_D_mean)

library(ggrepel)
library(scales)
options(scipen = 99)
ggplot(comparison,aes(x = delta_presidential_R,
	y = delta_presidential_D,
	color = delta_presidential_R - delta_presidential_D,
	label = state))+
	geom_label()+
	labs(x = "Change in Republican vote share",
		y = "Change in Democratic vote share")+
	facet_wrap(~office)+
	scale_x_continuous(labels = label_percent())+
	scale_y_continuous(labels = label_percent())+
	scale_color_gradientn(colors = c("darkblue","darkblue","blue","darkgray","darkred"))