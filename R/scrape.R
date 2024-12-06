# Inspired by <https://simonwillison.net/2020/Oct/9/git-scraping/>

get_html <- function() {
  url <- "http://www.westonlambert.com/available-work"
  read_html(url)
}

get_products <- function(html) {
  products <- html |>
    html_element("#productList") |>
    html_elements("a")

  gha_notice("Found {length(products)} products")

  if (length(products) == 0) {
    stop("No products found! Did the website change?")
  }
  products
}

parse_products <- function(products) {
  products <- data.frame(
    title = products |> html_element(".product-title") |> html_text(),
    price = products |>
      html_element(".product-price") |>
      html_text() |>
      gsub("[$,]", "", x = _) |>
      as.numeric(),
    sold_out = !(products |> html_element(".sold-out") |> html_text() |> is.na()),
    link = html_attr(products, "href")
  )
  gha_notice("Found {sum(!products$sold_out)} available products")

  gha_summary("### Current products\n")
  gha_summary(knitr::kable(products))
  products
}

update_products <- function(products) {
  data_dir <- system.file("output", package = "available-work")
  product_path <- file.path(data_dir, "products.csv")
  old <- read.csv(product_path)
  write.csv(products, product_path, row.names = FALSE)

  new <- subset(products, !sold_out & !link %in% old$link)
  gha_notice("Found {nrow(new)} new products")
}
