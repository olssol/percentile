require(shinydashboard);

dashboardPage(
    dashboardHeader(title = "Percentile Residual"),
    dashboardSidebar(
        sidebarMenu(
            id = "tabDist",
            menuItem("Gaussian",   tabName = "gaussian"),
            menuItem("Log-Normal", tabName = "lognormal"),
            menuItem("Gamma",      tabName = "gamma"),
            menuItem("Beta",       tabName = "beta"),
            menuItem("Binomial",   tabName = "binomial")
        ),
        div(style = "padding:15px",
            tabItems(
                tabItem("gaussian",
                        h4("Gaussian"),
                        withMathJax("True \\(\\mu = 0\\), \\(\\sigma = 1\\)"),
                        textInput("inGmu", withMathJax("\\(\\tilde{\\mu}\\)"), value = 0),
                        textInput("inGsigma", withMathJax("\\(\\tilde{\\sigma}\\)"), value = "1,1.5")),
                tabItem("lognormal",
                        h4("Log-Normal"),
                        withMathJax("True \\(\\mu = 0\\), \\(\\sigma = 1\\)"),
                        textInput("inLmu", withMathJax("\\(\\tilde{\\mu}\\)"), value = 0),
                        textInput("inLsigma", withMathJax("\\(\\tilde{\\sigma}\\)"), value = "1,1.2")),
                tabItem("gamma",
                        h4("Gamma"),
                        withMathJax("True \\(\\alpha = 1\\), \\(\\beta = 1\\)"),
                        textInput("inGalpha", withMathJax("\\(\\tilde{\\alpha}\\)"), value = 1),
                        textInput("inGbeta", withMathJax("\\(\\tilde{\\beta}\\)"), value = "1,1.5")),
                tabItem("beta",
                        h4("Beta"),
                        withMathJax("True \\(\\alpha = 1\\), \\(\\beta = 1\\)"),
                        textInput("inBalpha", withMathJax("\\(\\tilde{\\alpha}\\)"), value = 1),
                        textInput("inBbeta", withMathJax("\\(\\tilde{\\beta}\\)"), value = "1,1.5")),
                tabItem("binomial"))
            ),
        actionButton("btnUpdate", "Update")
       ),
    ##dashboardSidebar(disable = TRUE),
    dashboardBody(
        fluidRow(
            box(title = "Density Plot",
                solidHeader = TRUE,
                collapsible = TRUE,
                status = "warning",
                plotOutput("pltDen"),
                width = 11)
            ## box(title = "Control",
            ##     solidHeader = TRUE,
            ##     collapsible = TRUE,
            ##     status = "primary",
            ##     tabItems(
            ##         tabItem("gaussian",
            ##                 h2("Gaussian"),
            ##                 withMathJax(h4("True \\(\\mu = 0\\), \\(\\sigma = 1\\)")),
            ##                 textInput("inGmu", withMathJax("\\(\\tilde{\\mu}\\)"), value = 0, width = 200),
            ##                 textInput("inGsigma", withMathJax("\\(\\tilde{\\sigma}\\)"), value = 1, width = 200)),
            ##         tabItem("lognormal"),
            ##         tabItem("gamma"),
            ##         tabItem("beta"),
            ##         tabItem("binomial")
            ##     ))
            ## box (title = "Distributions",
            ##      solidHeader = TRUE,
            ##      collapsible = TRUE,
            ##      status = "primary",
            ##      fluidRow(
            ##          column(8,
            ##                 tabBox(id = "tabDist",
            ##                        side = "left",
            ##                        tabPanel("Gaussian",
            ##                                 withMathJax("True \\(\\mu = 0\\), \\(\\sigma = 1\\)"),
            ##                                 textInput("inGmu", withMathJax("\\(\\tilde{\\mu}\\)"),
            ##                                           value = 0, width = 200),
            ##                                 textInput("inGsigma", withMathJax("\\(\\tilde{\\sigma}\\)"),
            ##                                           value = 1, width = 200)))
            ##                 ),
            ##          column(4,
            ##                 actionButton("btnUpdate", "Update", width = "150px",
            ##                              style = "margin-top:200px"))
            ##      ),
            ##      width = 6)
        ),
        fluidRow(
            box(title = "ROC Plot",
                solidHeader = TRUE,
                collapsible = TRUE,
                status = "warning",
                plotOutput("pltROC"),
                width = 11)
        )
    )
)
