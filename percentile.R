
get.pars <- function(..., default = c(0,1)) {

    all.pars <- list(...);
    if (any(sapply(all.pars, is.null)))
        return(NULL);

    x <- lapply(all.pars, function(x) {
        sx  <- strsplit(as.character(x), ",");
        sapply(sx, function(y) {
            rst <- as.numeric(y);
            rst[!is.na(rst)]
        });
    })

    n.x <- sapply(x, length);

    if (any(0 == n.x))
        return(NULL);

    rst <- NULL;
    for (i in 1:length(x)) {
        rst <- cbind(rst, rep(x[[i]], length.out = max(n.x)));
    }

    ##remove default values
    if (length(default) == ncol(rst)) {
        inx <- apply(rst, 1, function(x) {!all(x == default)});
        rtn <- rbind(default);
        if (0 < length(inx)) {
            rtn <- rbind(rtn, rst[inx,]);
        }
    }

    ##return
    rtn
}

gr.gaussian <- function(alt.mu, alt.sigma, true.mu = 0, true.sigma = 1, n = 100000, skip = TRUE) {
    yk      <- rnorm(n = n, true.mu, true.sigma);
    rstar   <- (yk - alt.mu)/alt.sigma;

    finv.y  <- pnorm(yk, mean = alt.mu, sd = alt.sigma);
    rdagger <- qnorm(finv.y);

    cbind(rstar, rdagger);
}

gr.lognormal <- function(alt.mu, alt.sigma, true.mu = 0, true.sigma = 1, n = 100000, skip = TRUE) {
    yk0      <- rnorm(n = n, true.mu, true.sigma);
    yk       <- exp(yk0);

    til.mu   <- exp(alt.mu + alt.sigma^2/2);
    til.sig2 <- exp(2*alt.mu + alt.sigma^2) * (exp(alt.sigma^2) - 1)
    rstar    <- (yk - til.mu)/sqrt(til.sig2);

    finv.y   <- pnorm(yk0, mean = alt.mu, sd = alt.sigma);
    rdagger  <- qnorm(finv.y);

    cbind(rstar, rdagger);
}

gr.gamma <- function(alt.alpha, alt.beta, true.alpha = 1, true.beta = 1, n = 100000, skip = TRUE) {
    yk       <- rgamma(n = n, true.alpha, true.beta);

    til.mu   <- alt.alpha*alt.beta;
    til.sig2 <- alt.alpha * alt.beta^2;
    rstar    <- (yk - til.mu)/sqrt(til.sig2);

    finv.y   <- pgamma(yk, alt.alpha, alt.beta);
    rdagger  <- qnorm(finv.y);

    cbind(rstar, rdagger);
}

gr.beta <- function(alt.alpha, alt.beta, true.alpha = 1, true.beta = 1, n = 100000, skip = TRUE) {
    yk       <- rbeta(n = n, true.alpha, true.beta);

    til.mu   <- alt.alpha/(alt.alpha + alt.beta);
    til.sig2 <- alt.alpha * alt.beta / (alt.alpha + alt.beta)^2 / (alt.alpha + alt.beta + 1);
    rstar    <- (yk - til.mu)/sqrt(til.sig2);

    finv.y   <- pbeta(yk, alt.alpha, alt.beta);
    rdagger  <- qnorm(finv.y);

    cbind(rstar, rdagger);
}

get.r <- function(dist, ..., n = 100000) {

    ##all scenarios
    default <- switch(dist,
                      gaussian  = c(0,1),
                      lognormal = c(0,1),
                      gamma     = c(1,1),
                      beta      = c(1,1));

    r.pars <- get.pars(..., default = default);
    if (is.null(r.pars)) {
        return(NULL);
    }


    ##sampling
    rst <- list();
    for (i in 1:nrow(r.pars)) {
        rst[[i]]  <- do.call(paste("gr.", dist, sep = ""),
                             c(as.list(r.pars[i,]), n=n));
    }

    ##set scenarios
    colnames(r.pars) <- switch(dist,
                               gaussian  = c("mu", "sigma"),
                               lognormal = c("mu", "sigma"),
                               gamma     = c("alpha", "beta"),
                               beta      = c("alpha", "beta"));

    ##return
    list(pars = r.pars,
         rs   = rst);
}

plot.dens <- function(all.r, ncol = 2, add.n01 = TRUE, adjust = 1.2) {

    cnames <- colnames(all.r$pars);

    dens <- lapply(all.r$rs, function(x) {
        list(density(x[,1], adjust = adjust),
             density(x[,2], adjust = adjust))
    });

    ## lims <- sapply(dens, function(x) {
    ##     c(range(x[[1]]$x), range(x[[2]]$x), max(x[[1]]$y), max(x[[2]]$y));
    ## })

    ## ymax <- lims[5:6,];
    ## if (add.n01) {
    ##     ymax <- c(ymax, 0.4);
    ## }
    ## ymax <- max(ymax) * 1.05;

    if (1 < length(dens)) {
        par(mfrow = c(ceiling(length(dens)/ncol), ncol));
    }

    for (i in 1:length(dens)) {
        ##x limit
        lims.x <- c(range(dens[[i]][[1]]$x),
                    range(dens[[i]][[2]]$x));
        if (add.n01) {
            lims.x <- c(-3.5, 3.5, lims.x);
        }
        xlim <- range(lims.x);
        xlim <- xlim + c(-0.05*(xlim[2]-xlim[1]), 0.05*(xlim[2]-xlim[1]));

        ##main
        txt  <- paste("tilde(", cnames, ") = ", all.r$pars[i,], sep="");
        txt  <- paste(txt, collapse = ",");
        main <- txt;

        plot(dens[[i]][[1]]$x, dens[[i]][[1]]$y/max(dens[[i]][[1]]$y),
             type = "l", xlim = xlim, ylim = c(0, 1.05),
             col = "red", xlab = "Residual", ylab = "Density", main = main);
        lines(dens[[i]][[2]]$x, dens[[i]][[2]]$y/max(dens[[i]][[2]]$y), col = "blue");
        if (add.n01) {
            xs <- seq(xlim[1], xlim[2], by = 0.1);
            lines(xs, dnorm(xs)/max(dnorm(xs)), lty=1, col="black");
        }

        ## vertical line at 0
        lines(c(0,0), c(-1, 2), lty=2, col = "gray");

        ## legend
        if (1 == i) {
            l.txt <- c(as.expression(bquote(R^~"*")),
                       as.expression(bquote(R^~"\u2020")));
            l.col <- c("red", "blue");

            if (add.n01) {
                l.txt <- c(l.txt, "N(0,1)");
                l.col <- c(l.col, "black");
            }
            legend("topright", bty = "n", legend = l.txt, col = l.col, lty = 1);
        }
    }
}

get.roc <- function(rh0, rh1, n=100) {
    abs.rh0 <- abs(rh0);
    abs.rh1 <- abs(rh1);

    cuts    <- sample(c(rh0, rh1), n);
    cuts    <- sort(c(0, abs(cuts), Inf), decreasing = TRUE);

    rst <- NULL;
    for (i in cuts) {
        cur.sens <- mean(abs.rh1 > i);
        cur.mspe <- mean(abs.rh0 > i);
        rst      <- rbind(rst, c(cur.sens, cur.mspe));
    }

    rst
}

plot.roc <- function(all.r, ncol = 2, n=100) {

    fstep <- function(x) {
        stepfun(x[-nrow(x),1], x[,2]);
    }

    cnames <- colnames(all.r$pars);
    all.rs <- all.r$rs;
    if (length(all.rs) < 2) {
        return(NULL);
    }

    rstar0   <- all.rs[[1]][,1];
    rdagger0 <- all.rs[[1]][,2];

    lst.roc <- list();
    for (i in 2:length(all.rs)) {
        rstar1   <- all.rs[[i]][,1];
        rdagger1 <- all.rs[[i]][,2];

        lst.roc[[i-1]] <- list(get.roc(rstar0, rstar1, n=n),
                               get.roc(rdagger0, rdagger1, n=n));
    }

    if (1 < length(lst.roc)) {
        par(mfrow = c(ceiling(length(lst.roc)/ncol), ncol));
    }

    for (i in 1:length(lst.roc)) {
        ##main
        txt  <- paste("tilde(", cnames, ") = ", all.r$pars[i+1,], sep="");
        txt  <- paste(txt, collapse = ",");
        main <- txt;
        plot(NULL, NULL, xlim=c(0,1), ylim=c(0,1),
             xlab="Sensitivity",
             ylab = "1-Specificity",
             xaxs = "i", yaxs = "i",
             main = main);

        lines(fstep(lst.roc[[i]][[1]]), col = "red", do.points = FALSE);
        lines(fstep(lst.roc[[i]][[2]]), col = "blue", do.points = FALSE);
        lines(c(0,1), c(0,1), lty=2);
        if (1 == i) {
            l.txt <- c(as.expression(bquote(R^~"*")),
                       as.expression(bquote(R^~"\u2020")));
            l.col <- c("red", "blue");
            legend("bottomright", bty = "n", legend = l.txt, col = l.col, lty = 1);
        }
    }
}
