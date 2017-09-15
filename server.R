function(input, output, session) {

    source("percentile.R");

    userLog        <- reactiveValues();
    userLog$rdata  <- NULL;
    userLog$dist   <- NULL;

    observeEvent(input$btnUpdate, {
        userLog$dist <- input$tabDist;
        userLog$data <- switch(userLog$dist,
                               gaussian  = {get.r(userLog$dist, input$inGmu, input$inGsigma)},
                               lognormal = {get.r(userLog$dist, input$inLmu, input$inLsigma)},
                               gamma     = {get.r(userLog$dist, input$inGalpha, input$inGbeta)},
                               beta      = {get.r(userLog$dist, input$inBalpha, input$inBbeta)},)
    }, ignoreInit = TRUE)

    output$pltDen <- renderPlot({

        if (is.null(userLog$data))
            return(NULL);

        plot.dens(userLog$data);
    }, height = "auto")

    output$pltROC <- renderPlot({

        if (is.null(userLog$data))
            return(NULL);

        plot.roc(userLog$data);
    }, height = "auto")
}
