files = list.files('data-raw', '*.xlsx', full.names = TRUE)
library(dplyr)
options(width = 160)


poly_substract = function(x, y, order = 5){
    fit <- lm(y ~ poly(x, order))
    background <- predict(fit, data.frame(x = x))
    corrected_spectrum <- y - background
    return(corrected_spectrum)
}

normalize_data = function(wavenumber, signal, order = 5, from = 401, to = 1400, plot = FALSE){
    x = wavenumber
    y = signal
    if (length(x) != length(y)){
        stop("Length of wavenumber and signal is not equal.")
    }
    d = tibble(x = x, y = y) |> na.omit()
    y2 = poly_substract(d$x, d$y, order = order)
    xi = seq(from, to)
    y3 = signal::interp1(d$x, y2, xi, method = 'spline')
    y4 = scales::rescale(y3)
    if (plot){
        plot(x, y, type = 'l', ylim = range(c(y, y2, y3)), xlab = 'wavenumber', ylab = 'raman signal')
        lines(x, y2, col = 'blue')
        lines(xi, y3, col = 'red')
        lines(xi, y4, col = 'green')
    }
    return(y4)
}

read_file = function(file){
    data = openxlsx::read.xlsx(file, colNames = FALSE)  |> as_tibble()
    message("Processing ", file, ', which has ', ncol(data), ' columns.')
    list = lapply(1:(ncol(data)/2), function(i){
        x = data[[i*2-1]]
        y = data[[i*2]]
        normalize_data(x, y)
    })
    combined = do.call(cbind, list)
    return(t(combined))
}


data = lapply(files, read_file)
strain_name = basename(files) |> gsub(pattern = '.xlsx', replacement = '')
y = rep(0:(length(strain_name)-1), times = sapply(data, nrow))
X = do.call(rbind, data)

write.csv(X, file = 'raman_normalized_data.csv', row.names = FALSE)
write.csv(y, file = 'raman_group.csv', row.names = F)
