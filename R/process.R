#' check for collectl availability
#' @return logical(1)
#' @examples
#' cl_exists()
#' @export
cl_exists = function() {
  x = try(system2("collectl", "--help", stdout=TRUE))
  if (!inherits(x, "try-error") && length(x)>30) return(TRUE)
  FALSE
}

#' Start collectl with optional GPU monitoring
#' @importFrom processx process
#' @importFrom bgmeter bgmeter_start bgmeter_stop
#' @param target character(1) path; destination of collectl report
#' @param monitor_gpu logical(1) whether to enable GPU monitoring
#' @param gpu_monitor_type either "collectl" or "nvml" for GPU monitoring
#' @return instance of `Rcollectl_process` with monitoring components
#' @export
cl_start <- function(target = tempfile(), 
                     monitor_gpu = FALSE,
                     gpu_monitor_type = c("collectl", "nvml")) {
  gpu_monitor_type <- match.arg(gpu_monitor_type)
  
  args <- c("-scdmn", "-P", paste("-f", target, sep=""))
  
  # Initialize GPU monitoring
  gpu_monitor <- NULL
  bgmeter_process <- NULL
  gpu_metrics_file <- NULL
  
  if (monitor_gpu) {
    if (gpu_monitor_type == "collectl" && cl_gpu_exists()) {
      args <- c(args, "-sG")
    } else if (gpu_monitor_type == "nvml" && nvml_available()) {
      gpu_monitor <- GPUMonitor$new()
      if (requireNamespace("bgmeter", quietly = TRUE)) {
        gpu_metrics_file <- tempfile("gpumetrics", fileext = ".jsonl")
        # Define function to collect GPU metrics
        measure_gpu <- function() {
          metrics <- gpu_monitor$get_metrics()
          lapply(metrics, function(m) {
              list(
                  timestamp = as.character(Sys.time()),
                  metrics
              )
          })
        }

        # Start background GPU metrics collection every second
        bgmeter_process <- bgmeter_start(measure_gpu, 1L, gpu_metrics_file,"log.txt")
      } else {
        warning("bgmeter package not installed. GPU metrics will not be collected in the background.")
      }
    } else {
      warning("Requested GPU monitoring not available")
    }
  }
  
  proc <- try(processx::process$new("collectl", args = args))
  ans <- list(
    process = proc,
    target = target,
    node_name = Sys.info()[["nodename"]],
    date = format(Sys.Date(), "%Y%m%d"),
    gpu_monitor = gpu_monitor,
    gpu_monitor_type = if (monitor_gpu) gpu_monitor_type else NULL,
    bgmeter_process = bgmeter_process,
    gpu_metrics_file = gpu_metrics_file
  )
  class(ans) <- "Rcollectl_process"
  ans
}

#' Get GPU metrics from Rcollectl_process
#' @param proc Rcollectl_process object
#' @return list of GPU metrics or NULL if not monitoring
#' @export
cl_get_gpu_metrics <- function(proc) {
  if (!inherits(proc, "Rcollectl_process")) {
    stop("Not an Rcollectl_process object")
  }
  
  if (!is.null(proc$gpu_monitor)) {
    if (proc$gpu_monitor_type == "nvml") {
      return(proc$gpu_monitor$get_all_metrics())
    }
  }
  return(NULL)
}

#' print method for Rcollectl process
#' @param x an entity inheriting from "Rcollectl_process" S3 class
#' @param \dots not used
#' @return invisibly returns the input
#' @examples
#' example(cl_start)
#' @export
print.Rcollectl_process = function(x, ...) {
  cat("Rcollectl process object\n  ")
  print(x$process)
  cat("  Target: ", x$target, "\n")
  if (!is.null(x$gpu_metrics_file)) {
    cat("  GPU metrics file: ", x$gpu_metrics_file, "\n")
  }
  invisible(x)
}

#' stop collectl via processx interrupt
#' @param proc an entity inheriting from "Rcollectl_process" S3 class
#' @return invisibly returns the input
#' @examples
#' example(cl_start)
#' @export
cl_stop = function(proc) {
  stopifnot(inherits(proc, "Rcollectl_process"))
  proc$process$interrupt()
  if (!is.null(proc$bgmeter_process)) {
    bgmeter_stop(proc$bgmeter_process)
  }
  if (!is.null(proc$gpu_monitor)) {
    proc$gpu_monitor$finalize()
  }
  invisible(proc)
}

#' get full path to collectl report
#' @param proc an entity inheriting from "Rcollectl_process" S3 class
#' @return character(1) path to report
#' @examples
#' example(cl_start)
#' @export
cl_result_path = function(proc) {
  stopifnot(inherits(proc, "Rcollectl_process"))
  paste0(proc$target, "-", proc$node_name, "-", proc$date, ".tab.gz")
}

#' get path to GPU metrics file
#' @param proc an entity inheriting from "Rcollectl_process" S3 class
#' @return character(1) path to GPU metrics file or NULL if not available
#' @export
cl_gpu_metrics_path = function(proc) {
  stopifnot(inherits(proc, "Rcollectl_process"))
  proc$gpu_metrics_file
}

cl_plot_system_metrics <- function(proc) {
    
    # Read CPU data
    cpu_file <- paste0(proc[["target"]], "-", proc[["node_name"]], "-", proc[["date"]], ".tab.gz")
    cpu_data <- read.table(cpu_file, skip = 9, header = FALSE, comment.char = "#")
    
    # Create simple integer sequence for x-axis (1 to number of observations)
    x_values <- 1:nrow(cpu_data)
    
    # Extract CPU usage and memory usage
    cpu_metrics <- data.frame(
        x = x_values,
        cpu_usage = cpu_data$V10,
        memory_used_mb = cpu_data$V23 / 1024,  # Convert kB to MB
        memory_total_mb = cpu_data$V24 / 1024   # Convert kB to MB
    )
    
    # Read GPU data
    gpu_lines <- readLines(proc[["gpu_metrics_file"]])
    gpu_data <- lapply(gpu_lines, fromJSON)
    
    # Extract GPU metrics and use same x-values (assuming same number of observations)
    # If GPU has different number of observations, we'll use 1:n
    gpu_x_values <- 1:length(gpu_data)
    
    gpu_metrics <- data.frame(
        x = gpu_x_values,
        gpu_usage = sapply(gpu_data, function(x) x$gpu_util$`2`$gpu_util),
        gpu_memory_used_mb = sapply(gpu_data, function(x) x$gpu_util$`2`$memory_used),
        gpu_memory_total_mb = sapply(gpu_data, function(x) x$gpu_util$`2`$memory_total)
    )
    
    # Create the plot with 4 subplots with compact titles
    p1 <- ggplot(cpu_metrics, aes(x = x, y = cpu_usage)) +
        geom_point(size = 1) +
        labs(title = "CPU", x = "Time (seconds)", y = "Usage (%)") +
        theme_minimal() +
        theme(plot.title = element_text(size = 10, face = "bold"))
    
    p2 <- ggplot(cpu_metrics, aes(x = x, y = memory_used_mb)) +
        geom_point(size = 1) +
        labs(title = "CPUM", x = "Time (seconds)", y = "Memory (MB)") +
        theme_minimal() +
        theme(plot.title = element_text(size = 10, face = "bold"))
    
    p3 <- ggplot(gpu_metrics, aes(x = x, y = gpu_usage)) +
        geom_point(size = 1) +
        labs(title = "GPU", x = "Time (seconds)", y = "Usage (%)") +
        theme_minimal() +
        theme(plot.title = element_text(size = 10, face = "bold"))
    
    p4 <- ggplot(gpu_metrics, aes(x = x, y = gpu_memory_used_mb)) +
        geom_point(size = 1) +
        labs(title = "GPUM", x = "Time (seconds)", y = "Memory (MB)") +
        theme_minimal() +
        theme(plot.title = element_text(size = 10, face = "bold"))
    
    # Combine plots with compact layout
    gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 4)
}