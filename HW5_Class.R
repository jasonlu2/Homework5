## HW5 Class/Methods

setClass(
    Class = "sparse_numeric",
    slots = c(
        value = "numeric",
        pos = "integer",
        length = "integer"
    )
)

# Validity method
setValidity("sparse_numeric", function(object) {
    if (length(object@value) != length(object@pos)) {
        return("value and pos must have the same length")
    }
    if (any(object@pos < 1L) || any(object@pos > object@length)) {
        return("pos must be between 1 and length")
    }
    if (anyDuplicated(object@pos)) {
        return("pos must contain unique positions")
    }
    if (length(object@length) != 1L) {
        return("length must be a single integer")
    }
    return(TRUE)
})

# Generic functions
setGeneric("sparse_add", function(x, y, ...) standardGeneric("sparse_add"))
setGeneric("sparse_mult", function(x, y, ...) standardGeneric("sparse_mult"))
setGeneric("sparse_sub", function(x, y, ...) standardGeneric("sparse_sub"))
setGeneric("sparse_crossprod", function(x, y, ...) standardGeneric("sparse_crossprod"))

# Sparse arithmetic methods
setMethod("sparse_add", c("sparse_numeric", "sparse_numeric"), function(x, y, ...) {
    if (x@length != y@length) {
        stop("vectors must have the same length")
    }

    all_pos <- sort(union(x@pos, y@pos))

    x_map <- match(all_pos, x@pos)
    y_map <- match(all_pos, y@pos)

    new_values <- numeric(length(all_pos))
    new_values[!is.na(x_map)] <- new_values[!is.na(x_map)] + x@value[na.omit(x_map)]
    new_values[!is.na(y_map)] <- new_values[!is.na(y_map)] + y@value[na.omit(y_map)]

    non_zero <- new_values != 0
    if (any(non_zero)) {
        new("sparse_numeric",
            value = new_values[non_zero],
            pos = all_pos[non_zero],
            length = x@length)
    } else {
        new("sparse_numeric",
            value = numeric(0),
            pos = integer(0),
            length = x@length)
    }
})

setMethod("sparse_mult", c("sparse_numeric", "sparse_numeric"), function(x, y, ...) {
    if (x@length != y@length) {
        stop("vectors must have the same length")
    }

    common_pos <- intersect(x@pos, y@pos)
    if (length(common_pos) == 0) {
        return(new("sparse_numeric",
                  value = numeric(0),
                  pos = integer(0),
                  length = x@length))
    }

    x_idx <- match(common_pos, x@pos)
    y_idx <- match(common_pos, y@pos)

    new_values <- x@value[x_idx] * y@value[y_idx]

    non_zero <- new_values != 0
    if (any(non_zero)) {
        new("sparse_numeric",
            value = new_values[non_zero],
            pos = common_pos[non_zero],
            length = x@length)
    } else {
        new("sparse_numeric",
            value = numeric(0),
            pos = integer(0),
            length = x@length)
    }
})

setMethod("sparse_sub", c("sparse_numeric", "sparse_numeric"), function(x, y, ...) {
    if (x@length != y@length) {
        stop("vectors must have the same length")
    }

    all_pos <- sort(union(x@pos, y@pos))

    x_map <- match(all_pos, x@pos)
    y_map <- match(all_pos, y@pos)

    new_values <- numeric(length(all_pos))
    new_values[!is.na(x_map)] <- new_values[!is.na(x_map)] + x@value[na.omit(x_map)]
    new_values[!is.na(y_map)] <- new_values[!is.na(y_map)] - y@value[na.omit(y_map)]

    non_zero <- new_values != 0
    if (any(non_zero)) {
        new("sparse_numeric",
            value = new_values[non_zero],
            pos = all_pos[non_zero],
            length = x@length)
    } else {
        new("sparse_numeric",
            value = numeric(0),
            pos = integer(0),
            length = x@length)
    }
})

setMethod("sparse_crossprod", c("sparse_numeric", "sparse_numeric"), function(x, y, ...) {
    if (x@length != y@length) {
        stop("vectors must have the same length")
    }

    common_pos <- intersect(x@pos, y@pos)
    if (length(common_pos) == 0) {
        return(0)
    }

    x_idx <- match(common_pos, x@pos)
    y_idx <- match(common_pos, y@pos)

    sum(x@value[x_idx] * y@value[y_idx])
})

setMethod("+", c("sparse_numeric", "sparse_numeric"), function(e1, e2) sparse_add(e1, e2))
setMethod("*", c("sparse_numeric", "sparse_numeric"), function(e1, e2) sparse_mult(e1, e2))
setMethod("-", c("sparse_numeric", "sparse_numeric"), function(e1, e2) sparse_sub(e1, e2))

# Coercion methods
setAs("numeric", "sparse_numeric", function(from) {
    non_zero <- from != 0
    if (any(non_zero)) {
        new("sparse_numeric",
            value = from[non_zero],
            pos = which(non_zero),
            length = length(from))
    } else {
        new("sparse_numeric",
            value = numeric(0),
            pos = integer(0),
            length = length(from))
    }
})

setAs("sparse_numeric", "numeric", function(from) {
    result <- numeric(from@length)
    if (length(from@pos) > 0) {
        result[from@pos] <- from@value
    }
    result
})

# Show method
setMethod("show", "sparse_numeric", function(object) {
    cat("Sparse numeric vector of length", object@length, "\n")
    if (length(object@pos) == 0) {
        cat("All elements are zero\n")
    } else {
        cat("Non-zero elements:\n")
        for (i in seq_along(object@pos)) {
            cat("  [", object@pos[i], "] = ", object@value[i], "\n", sep = "")
        }
    }
})

# Plot method
setMethod("plot", c("sparse_numeric", "sparse_numeric"), function(x, y, ...) {
    if (x@length != y@length) {
        stop("Cannot plot vectors of different lengths")
    }

    overlap_pos <- intersect(x@pos, y@pos)

    plot(1, type = "n", xlim = c(1, x@length), ylim = c(-max(c(x@value, y@value), 0.1), max(c(x@value, y@value), 0.1)),
         xlab = "Position", ylab = "Value", main = "Sparse Vector Comparison")

    if (length(x@pos) > 0) {
        points(x@pos, x@value, col = "blue", pch = 16, cex = 1.5)
    }

    if (length(y@pos) > 0) {
        points(y@pos, y@value, col = "red", pch = 17, cex = 1.5)
    }

    if (length(overlap_pos) > 0) {
        x_overlap_idx <- match(overlap_pos, x@pos)
        y_overlap_idx <- match(overlap_pos, y@pos)
        points(overlap_pos, x@value[x_overlap_idx], col = "green", pch = 15, cex = 2)
        points(overlap_pos, y@value[y_overlap_idx], col = "green", pch = 15, cex = 2)
    }

    legend("topright", legend = c("Vector x", "Vector y", "Overlap"),
           col = c("blue", "red", "green"), pch = c(16, 17, 15), cex = 0.8)
})

# Additional sparsity method
setGeneric("sparsity", function(x) standardGeneric("sparsity"))

setMethod("sparsity", "sparse_numeric", function(x) {
    (length(x@pos) / x@length) * 100
})

