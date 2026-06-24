# Module/Script file heavy
peeky::peek_shinylive_app(
    "https://shiny.thecoatlessprofessor.com/probability-distribution-explorer/",
    output_dir = file.path(tempdir(), "probability-distribution-explorer"))

# Resource rich
peeky::peek_shinylive_app(
    "https://jeanjoe.net/peeky_example/",
    output_dir = file.path(tempdir(), "peeky-example-data"))

