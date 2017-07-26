library(shiny)

ui <- fluidPage(
  actionButton("go", "Create Password!"),
  numericInput("n", "Password Length", 10),
  textOutput("pwd")
)


password_machine <- function(len)  {
  spcl <- c("!",  "#", "$", "%", "&", "(", ")", "*",  "+", "-", "/", ":",
            ";", "<", "=", ">", "?", "@", "[", "^", "_", "{", "|", "}", "~")
  
  # start by choosing 1 of each char type, to ensure every password has coverage
  s <- sample(spcl, 1)
  l <- sample(letters, 1)
  L <- sample(LETTERS, 1)
  n <- sample(1:9, 1)
  
  # pool chars and assign probs for sampling
  comb <- c(1:9, spcl, letters, LETTERS)
  p <- c(rep(0.035, 9), rep(0.015, 25), rep(0.025, 52))
  
  # sample additional chars to complete 'len'
  base <- c(s, l, L, n, sample(comb, len-4, TRUE, prob = p))
  
  # randomize char order and paste together
  password <- paste0(sample(base), collapse = "")
  
  return(password)
}


server <- function(input, output) {
  
  genPassword <- eventReactive(input$go, {
    password_machine(input$n)
  })
  
  output$pwd <- renderText({
  genPassword()
  })
}

shinyApp(ui = ui, server = server)
