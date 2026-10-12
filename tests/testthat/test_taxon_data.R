library(testthat)

sp_names <- c("Species B", "Species A")

fake_taxonomy <- function(sp_names) {
  data.frame(
    scientific_name = sp_names,
    phylum = "Ascomycota",
    class = "Lecanoromycetes",
    order = "Lecanorales",
    family = "Parmeliaceae",
    genus = paste0("Genus", seq_along(sp_names))
  )
}

fake_ecology_traits <- function(sp_names) {
  data.frame(
    scientific_name = sp_names,
    substrata = "bark",
    poleotolerance_min = 1,
    poleotolerance_max = seq_along(sp_names) + 2
  )
}

fake_regions_distribution <- function(sp_names) {
  data.frame(
    scientific_name = sp_names,
    abruzzo = 1,
    veneto = seq_along(sp_names) %% 2
  )
}

fake_ecoregions_distribution <- function(sp_names) {
  data.frame(
    scientific_name = sp_names,
    alpine = "rare",
    dry_mediterranean = paste0("value", seq_along(sp_names))
  )
}

describe("italic_taxon_data function", {
  test_that("keeps all the data columns and the scientific name once", {
    local_mocked_bindings(
      italic_taxonomy = fake_taxonomy,
      italic_ecology_traits = fake_ecology_traits,
      italic_regions_distribution = fake_regions_distribution,
      italic_ecoregions_distribution = fake_ecoregions_distribution
    )

    result <- italic_taxon_data(sp_names)

    expect_equal(
      colnames(result),
      c("scientific_name", "phylum", "class", "order", "family", "genus",
        "substrata", "poleotolerance_min", "poleotolerance_max",
        "abruzzo", "veneto",
        "alpine", "dry_mediterranean")
    )
    expect_equal(result$scientific_name, sp_names)
    expect_equal(result$genus, c("Genus1", "Genus2"))
    expect_equal(result$poleotolerance_max, c(3, 4))
    expect_equal(result$veneto, c(1, 0))
    expect_equal(result$dry_mediterranean, c("value1", "value2"))
  })

  test_that("works with a single name", {
    local_mocked_bindings(
      italic_taxonomy = fake_taxonomy,
      italic_ecology_traits = fake_ecology_traits,
      italic_regions_distribution = fake_regions_distribution,
      italic_ecoregions_distribution = fake_ecoregions_distribution
    )

    result <- italic_taxon_data("Species A")

    expect_equal(nrow(result), 1)
    expect_equal(ncol(result), 13)
    expect_equal(sum(colnames(result) == "scientific_name"), 1)
  })
})
