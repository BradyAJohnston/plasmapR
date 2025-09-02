test_that("get features", {
  testthat::expect_equal(
    .get_label('                      /regulatory_class="terminator"'),
    "regulatory_class"
  )
  testthat::expect_equal(
    .get_value('                      /regulatory_class="terminator"'),
    "terminator"
  )
  expect_equal(
    .get_label('                     /note="efficient rho-independent terminator B1006"'),
    "note"
  )
  expect_equal(
    .get_value('                     /note="efficient rho-independent terminator B1006"'),
    'efficient rho-independent terminator B1006'
  )

  expect_equal(
    .get_start_end("      protein_bind    82..86"),
    c(82, 86)
  )

  expect_equal(
    .get_start_end("      primer_bind     complement(170..186)"),
    c(170, 186)
  )
  expect_equal(
    .get_feature_type("      primer_bind     complement(170..186)"),
    "primer_bind"
  )

  expect_equal(
    .get_direction("      primer_bind     complement(170..186)"),
    -1
  )
  expect_equal(
    .get_direction("      protein_bind    82..86"),
    1
  )
})

test_that('expections', {
  expect_true(
    .is_label_start('                     /regulatory_class="ribosome_binding_site"')
  )
  expect_false(
    .is_feature_start('                     /regulatory_class="ribosome_binding_site"')
  )
  expect_true(
    .is_feature_start('     regulatory      70..81')
  )
  expect_false(
    .is_label_start('     regulatory      70..81')
  )
})

test_that("Extract Lines", {
  fl <- system.file("extdata", "sequence.gb", package = "plasmapR")
  lines <- readLines(fl)
  expect_equal(
    sum(.get_line_types(lines) == "FEATURES"),
    59
  )
})

test_that("Extract over origin", {
  line <- " CDS             join(4891..5096,1..751)"
  values <- .get_start_end(line)
  expect_equal(
    values,
    c(4891, 751)
  )
  direction <- .get_direction(line)
  expect_equal(
    direction,
    1
  )
})

test_that("Origin-spanning feature detection and offset", {
  # Create test data with origin-spanning feature
  test_features <- list(
    list(
      type = "CDS",
      name = "TurboID", 
      start_end = c(4891, 751),
      direction = 1
    ),
    list(
      type = "gene",
      name = "normal_gene",
      start_end = c(1000, 2000), 
      direction = 1
    )
  )
  
  # Test with bp = 5096 (plasmid length)
  df <- .feature_list_to_df(test_features, bp = 5096)
  
  # After processing, no feature should have start > end (they should all be fixed)
  expect_true(all(df$start <= df$end))
  
  # Check that the TurboID feature spans correctly
  turboid <- df[df$name == "TurboID", ]
  expect_true(nrow(turboid) == 1)
  expect_true(turboid$start < turboid$end)
  expect_true(turboid$end > turboid$start)
})

test_that("Multiple origin-spanning features", {
  test_features <- list(
    list(
      type = "CDS",
      name = "feature1",
      start_end = c(4500, 500),
      direction = 1
    ),
    list(
      type = "CDS", 
      name = "feature2",
      start_end = c(4800, 300),
      direction = 1
    ),
    list(
      type = "gene",
      name = "normal_feature",
      start_end = c(1000, 2000),
      direction = 1
    )
  )
  
  df <- .feature_list_to_df(test_features, bp = 5000)
  
  # All features should have valid coordinates after offset
  expect_true(all(df$start <= df$end))
  expect_true(all(df$start >= 0))
  expect_true(all(df$end >= df$start))
})

test_that("Complement origin-spanning features", {
  test_features <- list(
    list(
      type = "CDS",
      name = "complement_feature", 
      start_end = c(4891, 751),
      direction = -1  # complement
    )
  )
  
  df <- .feature_list_to_df(test_features, bp = 5096)
  
  # Complement features should not trigger offset logic
  # (only direction = 1 features should)
  complement_feat <- df[df$name == "complement_feature", ]
  expect_true(nrow(complement_feat) == 1)
})

test_that("Real plasmid file with origin-spanning feature", {
  # Test the actual problematic file
  file_path <- system.file("extdata", "559763_pLann.txt", package = "plasmapR")
  
  if (file.exists(file_path)) {
    plasmid <- read_gb(file_path)
    df <- as.data.frame(plasmid)
    
    # Find the TurboID feature
    turboid <- df[df$name == "TurboID", ]
    expect_true(nrow(turboid) == 1)
    
    # After processing, it should have valid coordinates
    expect_true(turboid$start < turboid$end)
    expect_true(turboid$start >= 0)
    expect_true(turboid$end <= max(df$end))
    
    # The feature should span a reasonable length
    # Original was join(4891..5096,1..751) = (5096-4891) + 751 = 956 bp
    expected_length <- (5096 - 4891 + 1) + 751  # 957 bp
    actual_length <- turboid$end - turboid$start + 1
    
    # Allow some tolerance for offset calculations
    expect_true(abs(actual_length - expected_length) <= 10,
                info = paste("Expected ~", expected_length, "got", actual_length))
  } else {
    skip("Test file not found")
  }
})
