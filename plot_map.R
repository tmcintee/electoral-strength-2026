# Designed to be a similar style to Split Ticket's WAR map for easy comparison.

library(tidyverse)
library(sf)

offices    <- c("Governor", "Senate")

# ---- one score per state x office, from the Democratic side --------------------------------------------------
# D and R scores are near mirror images in a D-vs-R race; keep the D row, and flip the R row where there's no D
# (e.g. a Dem-backed independent race, if your table carries it under REP only).
scores <- four_strength_measures %>%
	filter(office %in% offices, party_std %in% c("DEM", "REP")) %>%
	group_by(state, office) %>%
	filter(party_std == "REP" | !any(party_std == "REP")) %>%
	mutate(score = strength)
# ---- state shapes: usmap gives Alaska and Hawaii as insets; maps (fallback) is lower 48 only ------------------
get_states <- function() 
{
	if (requireNamespace("usmap", quietly = TRUE)) 
	{
		return_map <- usmap::us_map(regions = "states") %>% transmute(state = full, abb = abbr)
	} 
	else 
	{
		message("usmap not installed - drawing the lower 48 only (install.packages('usmap') for AK/HI insets)")
		return_map <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE)) %>%
			st_transform(5070) %>%
			mutate(state = tools::toTitleCase(sub(":.*", "", ID))) %>%      # 'michigan:north' etc. -> one state
			mutate(abb = state.abb[match(state, state.name)])
	}
	return(return_map)
}
states <- get_states()

map_df <- states %>%
	crossing(office = offices) %>%                 # every state appears in every facet
	left_join(scores, by = c("state", "office")) %>%
	st_as_sf() %>%
	mutate(label = if_else(is.na(score), NA_character_,
		paste0(abb, "\n", if_else(score >= 0, "D+", "R+"), formatC(abs(score), format = "f", digits = 1))))

lim <- max(abs(map_df$score), na.rm = TRUE)

# ---- label positions: inside the state, or (small Northeastern states) in a column off the coast -------------
small <- c("VT", "NH", "MA", "RI", "CT", "NJ", "DE", "MD", "DC")
label_pts <- map_df %>%
	select(state, office) %>%
	mutate(row = row_number()) %>%
	st_cast("POLYGON", warn = FALSE) %>%
	mutate(area = as.numeric(st_area(.))) %>%
	group_by(row) %>%
	slice_max(area, n = 1, with_ties = FALSE) %>%
	ungroup() %>%
	arrange(row) %>%
	st_point_on_surface()

xy <- st_coordinates(label_pts)
bb <- st_bbox(map_df)
labs_df <- map_df %>%
	st_drop_geometry() %>%
	mutate(px = xy[, 1], py = xy[, 2],
		value = formatC(score, format = "f", digits = 0),
		outside = abb %in% small) %>%
	filter(!is.na(score))

inside  <- labs_df %>% filter(!outside) %>%
	mutate(txt = paste0(abb, "\n", value),
		ink = if_else(abs(score) > lim * 0.55, "white","black"))          # white text on dark fills
outside <- labs_df %>% filter(outside) %>%
	group_by(office) %>%
	arrange(desc(py), .by_group = TRUE) %>%                                   # keep north-to-south order
	mutate(lx = bb[["xmax"]] + 0.06 * (bb[["xmax"]] - bb[["xmin"]]),
		ly = seq(bb[["ymax"]] - 0.30 * (bb[["ymax"]] - bb[["ymin"]]),
			by = -0.065 * (bb[["ymax"]] - bb[["ymin"]]), length.out = n()),
		txt = paste(abb, value)) %>%
	ungroup()

ggplot(map_df) +
	geom_sf(aes(fill = score), color = "black", linewidth = 0.15, alpha = 0.8) +
	geom_text(data = inside, aes(px, py, label = txt, colour = ink),
		size = 2.4, lineheight = 0.85, fontface = "bold") +
	geom_segment(data = outside, aes(x = lx, y = ly, xend = px, yend = py),
		color = "darkgray", linewidth = 0.25, alpha = 0.8) +
	geom_text(data = outside, aes(lx, ly, label = txt), hjust = 0, nudge_x = 0.006 * (bb[["xmax"]] - bb[["xmin"]]),
		size = 2.4, color = "black") +
	scale_color_identity() +
	scale_fill_gradientn(colors = c("darkblue","darkblue","blue","lightblue","white","pink","red","darkred","darkred"),
		na.value = "darkgray",
		name = NULL) +
	coord_sf(xlim = c(bb[["xmin"]], bb[["xmax"]] + 0.17 * (bb[["xmax"]] - bb[["xmin"]])), expand = FALSE, datum = NA) +
	facet_wrap(~office, nrow = 1) +
	labs(title = "Candidate strength by state",
		subtitle = "Composite score based on within-ballot, within-state, within-office, and vs-presidential",
		caption = "Grey: No race or unscored for technical reasons") +
	theme_void(base_size = 11) +
	theme(legend.position = "bottom", legend.key.width = unit(2.5, "cm"),
		strip.text = element_text(face = "bold", size = 12, margin = margin(b = 4)),
		plot.title = element_text(face = "bold", size = 16), plot.background = element_rect(fill = "white", color = NA))