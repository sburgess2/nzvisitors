library(tidyverse)
library(circlize)
activities <- read_csv("data/activities.csv")

activities1 <- activities %>%
  mutate(
    activity = str_replace_all(activity, "Skiing, snowboarding, sledging or other snow sport", "Snow sport"),
    activity = str_replace_all(activity, "Went for a walk, hike, trek or tramp", "Hiking"),
    activity = str_replace_all(activity, "A public museum or art gallery", "Museum or art gallery"),
    activity = str_replace_all(activity, "A wildlife sanctuary (a sanctuary for New Zealand's endangered species)", "Wildlife sanctuary"),
    activity = str_replace_all(activity, "Extreme ride e.g. Luge, fly-by-wire, zorbing, flying fox/zip-lining", "Extreme ride")
  ) %>%
  filter(activity %in% c(
    "Cycling", "Snow sport", "Hiking", "Museum or art gallery", "Hot pools", "Extreme ride", "Zoo or wildlife park",
    "Fishing", "Playing golf", "Jet-boating", "Bungy jumping", "Rafting, canoeing, kayaking", "Stargazing"
  )) %>%
  select(response_id, activity) %>%
  distinct()


# https://stackoverflow.com/questions/34670145/generating-an-edge-list-from-id-and-grouping-vectors/34670262tuto

activities2 <- activities1 %>%
  group_by(response_id) %>%
  filter(n() >= 2) %>%
  group_by(response_id) %>%
  do(data.frame(t(combn(.$activity, 2)), stringsAsFactors = FALSE)) %>%
  ungroup() %>%
  mutate(
    # if Hiking is in X2, swap
    across(c(X1, X2), as.character), # ensure strings
    tmp1 = ifelse(X2 == "Hiking", X2, X1),
    tmp2 = ifelse(X2 == "Hiking", X1, X2),
    X1   = ifelse(tmp1 == "Hiking", tmp1, X1),
    X2   = ifelse(tmp1 == "Hiking", tmp2, X2)
  ) %>%
  select(-tmp1, -tmp2)



# from https://r-graph-gallery.com/123-circular-plot-circlize-package-2.html
edges <- with(activities2, table(X1, X2))


# Make the circular plot
chordDiagram(adjacencyData, transparency = 0.5)
circos.clear()

chordDiagram(adjacencyData, transparency = 0.5, order = c("Bungy jumping", "Cycling", "Extreme ride", "Playing golf", "Fishing", "Hiking", "Jet-boating", "Rafting, canoeing, kayaking", "Snow sport", "Stargazing", "Hot pools", "Zoo or wildlife park", "Museum or art gallery"))
circos.clear()

# Chatgpt was used for the following section of code

activity <- c(
  "Bungy jumping", "Cycling", "Extreme ride", "Playing golf", "Hiking", "Fishing",
  "Jet-boating", "Rafting, canoeing, kayaking", "Snow sport", "Stargazing"
)

place <- c("Zoo or wildlife park", "Hot pools")

experience <- c("Museum or art gallery")

# Combine into one order
desired_order <- c(activity, place, experience)

# Create a group label for each element in desired_order
group <- c(
  rep("Activity", length(activity)),
  rep("Place", length(place)),
  rep("Experience", length(experience))
)
gap_vector <- rep(5, length(desired_order))

# add bigger gaps at the end of each group
gap_vector[which(group == "Activity") %>% max()] <- 15
gap_vector[which(group == "Place") %>% max()] <- 15

gap_vector[which(group == "Experience") %>% max()] <- 15

# circos.par is used for setting parameters for the circular layout
# gap.after controls the after each sector
circos.par(gap.after = gap_vector)
chordDiagram(adjacencyData,
  order = desired_order,
  transparency = 0.5
)
ciros.clear()

# alternatively
circos.par(gap.after = c(rep(5, 9), 15, 5, 15, 15))
chordDiagram(adjacencyData, order = c("Bungy jumping", "Cycling", "Extreme ride", "Playing golf", "Fishing", "Hiking", "Jet-boating", "Rafting, canoeing, kayaking", "Snow sport", "Stargazing", "Hot pools", "Zoo or wildlife park", "Museum or art gallery"))
circos.clear()

# Example below from https://jokergoo.github.io/circlize_book/book/the-chorddiagram-function.html
# repeat a gap of 5 for repeated nrow-1, followed by a gap of 15, followed by a gap of 5 repeated ncol-1 followed by a gap of 15
circos.par(gap.after = c(rep(5, nrow(activities2) - 1), 15, rep(5, ncol(activities2) - 1), 15))
chordDiagram(mat)

library(paletteer)
paletteer_d("ggsci::deep_purple_material")
paletteer_d("ggsci::pink_material")

grid.col <- c("Bungy jumping" = "#d73027", "Cycling" = "#fdae61", "Extreme ride" = "#f46d43", "Playing golf" = "#ffffbf", "Fishing" = "#e0f3f8", "Hiking" = "#1a9850", "Jet-boating" = "#abd9e9", "Rafting, canoeing, kayaking" = "#4575b4", "Snow sport" = "grey", "Stargazing" = "#fee090", "Hot pools" = "#B39DDBFF", "Zoo or wildlife park" = "#311B92FF", "Museum or art gallery" = "#F48FB1FF")

circos.par(gap.after = c(rep(5, 9), 15, 5, 15, 15))
chordDiagram(edges, grid.col = grid.col, order = c("Bungy jumping", "Cycling", "Extreme ride", "Playing golf", "Fishing", "Hiking", "Jet-boating", "Rafting, canoeing, kayaking", "Snow sport", "Stargazing", "Hot pools", "Zoo or wildlife park", "Museum or art gallery"))
circos.clear()

# Showing each origin individually
# From https://stackoverflow.com/questions/79469693/how-to-fade-out-part-of-a-chord-diagram
all_areas <- union(rownames(edges), colnames(edges))
greygrid <- setNames(rep("#D1C7B5", length(all_areas)), all_areas)

par(mfrow = c(3, 4))

# The following two code chunk were used for only colouring outgoing links
all_areas %>%
  map(function(xx) {
    greygrid[xx] <- "#1F6683"
    chordDiagram(adjacencyData,
      transparency = 0.5,
      grid.col = greygrid
    )
    title(xx)
  })

lapply(all_areas, \(xx){
  greygrid[xx] <- "#1F6683"
  chordDiagram(adjacencyData,
    transparency = 0.5,
    grid.col = greygrid
  )
  title(xx)
})

# I then used Claude to alter this code so that both the outgoing and incoming links were blue

all_areas %>%
  map(function(xx) {
    # Set grid color for the focal area
    current_grid <- greygrid
    current_grid[xx] <- "#1F6683"

    # Create color matrix for links
    link_colors <- matrix("#80808080",
      nrow = nrow(adjacencyData), ncol = ncol(adjacencyData),
      dimnames = list(rownames(adjacencyData), colnames(adjacencyData))
    )

    # Color outgoing links (from xx to anywhere) in blue
    if (xx %in% rownames(adjacencyData)) {
      link_colors[xx, ] <- "#1F668380"
    }

    # Color incoming links (from anywhere to xx) in blue
    if (xx %in% colnames(adjacencyData)) {
      link_colors[, xx] <- "#1F668380"
    }

    chordDiagram(adjacencyData,
      transparency = 0.5,
      grid.col = current_grid,
      col = link_colors
    )
    title(xx)
  })

# Takahe palette
c("#DD3C51", "#313657", "#1F6683", "#6C90B9", "#D1C7B5")
