test_that("rendering functions work", {
  penguins <- palmerpenguins::penguins
  g <- ggplot2::ggplot(penguins, ggplot2::aes(x = species, y = bill_length_mm, fill = species)) +
    ggplot2::geom_violin()

  # render_svg
  # This is supposed to create an svg from a ggplot object or alternatively some ggplot code
  # And show it in the browser
  # TODO need a test that works, e.g. snapshots, I have to look into it
  # Test general renderin
  vizard::render_svg(g)
  # Test changing svg width, height and scaling (refer to svglite specs)
  vizard::render_svg(g, height = 4)
  vizard::render_svg(g, width = 4)
  vizard::render_svg(g, scaling = 0.5)
  # Test element width and height
  vizard::render_svg(g, element_height = "100vh")
  vizard::render_svg(g, element_width = "100vh")
  vizard::render_svg(g, element_width = "100vh", element_height = "50vh", scaling = 2)

  # Test changing font family
  vizard::render_svg(g,
    web_fonts = "https://fonts.googleapis.com/css2?family=Nunito+Sans:wght@700&family=Playball&display=swa",
    font_family = "Playball"
  )

  # This should also work if we pass a expression that can be evaluated to a ggplot object
  # This allows for other plotting engines to work, e.g. ComplexHeatmap or base R
  g_expr <- rlang::expr(ggplot2::ggplot(penguins, ggplot2::aes(x = species, y = bill_length_mm, fill = species)) +
    ggplot2::geom_violin())
  vizard::render_svg(g_expr)

  base_expr <- rlang::expr(plot(1, 1))
  vizard::render_svg(base_expr)

  ht_expr <- rlang::expr(ComplexHeatmap::Heatmap(1:10))
  vizard::render_svg(ht_expr)

  # Expect errors
  # Wrong data type for g
  testthat::expect_error(vizard::render_svg(g = NULL))
  testthat::expect_error(vizard::render_svg(g = "Hello"))
  testthat::expect_error(vizard::render_svg(g = g, heigth = "10"))
  testthat::expect_error(vizard::render_svg(g = g, width = "10"))
  testthat::expect_error(vizard::render_svg(g = g, scaling = "10"))
  testthat::expect_error(vizard::render_svg(g = g, element_height = 10))
  testthat::expect_error(vizard::render_svg(g = g, element_width = 10))
})
