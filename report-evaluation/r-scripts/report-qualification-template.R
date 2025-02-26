#' @title report-qualification-template
#' @description
#' Function that evaluates a model and generate its Qualification Report for `Open-Systems-Pharmacology/` sub-repository
#' @param repoName Name of Github OSP repository from which to get the Model
#' @param modelVersion Tag version of the model OSP repository default: '1.0'
#' @param snaphotName Name of the snapshot (.json) file default use same name as modelName
#' @param workflowScript Path of workflow R script that creates the function to run the qualification if not default
#' @param additionalProjectURLs URL of additional project snapshots to export as pksim5 projects
#' @param pkSimVersion Path a csv file defining the tools and versions to install
#' @param qualificationFrameworkVersion Version of the Qualification Framework
#' @param saveModelFile Save pksim5 model file
#' @param pdfReport Save the report as PDF
#' @param wordReport Save the report as Word document
runEvaluationReport <- function(
  repoName, 
  modelVersion, 
  snaphotName = repoName, 
  workflowScript = NULL,
  additionalProjectURLs = NULL,
  pkSimVersion = '11.3.208',
  qualificationFrameworkVersion = '3.2',
  saveModelFile = TRUE,
  pdfReport = TRUE,
  wordReport = FALSE
  ) {
  # In case clean-up directory
  library(ospsuite.reportingengine)
  ospsuite::clearMemory(clearSimulationsCache = TRUE)

  # TODO: this may require a better handling once the setup action is nicely implemented
  qualificationRunnerFolder <- normalizePath("QualificationRunner/QualificationRunner", winslash = "/")
  pkSimPortableFolder <- normalizePath("PK-Sim/PK-Sim", winslash = "/")
  pkSimPath <- file.path(pkSimPortableFolder, "PKSim.CLI.exe")
  
  qualificationProject <- repoName
  modelName <- snaphotName
  snapshotFile <- paste0(modelName, ".json")
  versionInfo <- QualificationVersionInfo$new(modelVersion, paste(head(unlist(strsplit(pkSimVersion, "\\.")), 2), collapse = "."), qualificationFrameworkVersion)
  
  workingDirectory <- normalizePath(modelName, mustWork = FALSE, winslash = "/")
  # Clean up because of potential rebase
  unlink(workingDirectory, recursive = TRUE)
  
  # Load repository content and clean up downloads
  download.file(
    paste0("https://github.com/Open-Systems-Pharmacology/", qualificationProject, "/archive/refs/tags/v", modelVersion, ".zip"),
    destfile = "archive.zip"
  )
  unzip("archive.zip", exdir = "archive")
  unlink("archive.zip")
  file.copy(
    list.files("archive", pattern = qualificationProject, full.names = TRUE), 
    getwd(), 
    recursive = TRUE
    )
  file.rename(from = list.files(pattern = qualificationProject), to = modelName)
  unlink("archive", recursive = TRUE)
  
  #' @description Use Workflow name to run the qualification
  setwd(workingDirectory)
  qualificationPath <- ifelse(
    any(is.null(workflowScript), is.na(workflowScript), workflowScript %in% ""),
    # If note specified, use default evaluation/workflow.R
    list.files(recursive = TRUE, pattern = "workflow.R", full.names = TRUE, ignore.case = TRUE),
    workflowScript
  )
  source(qualificationPath)
  
  # Needs to be run from same directory as workflow.R
  setwd(dirname(qualificationPath))
  createQualificationReport(
    qualificationRunnerFolder = qualificationRunnerFolder,
    pkSimPortableFolder = pkSimPortableFolder,
    createWordReport = wordReport,
    versionInfo = versionInfo
  )
  # Include report only in the model folder and clean up qualification
  setwd(workingDirectory)
  reportPaths <- list.files(pattern = "report.md", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  copyReport(
    from = tail(reportPaths, 1), 
    to = paste0(modelName, "_evaluation_report.md"), 
    copyWordReport = FALSE
    )
  unlink(dirname(qualificationPath), recursive = TRUE)
  reportPath <- file.path(workingDirectory, paste0(modelName, "_evaluation_report.md"))
  
  # Convert markdown to html and then to PDF
  setwd("..")
  if(pdfReport){
    knitr::pandoc(reportPath, paste("html", "--embed-resources", "--standalone", "-c \"osp.css\""))
    cmdLine <- paste(
    'chromehtml2pdf',
    paste0('--out="', gsub(pattern = ".md", ".pdf", reportPath), '"'),
    "--displayHeaderFooter true",
    "--format A4", "--marginTop 10mm", "--marginBottom 10mm", "--marginLeft 10mm", "--marginRight 10mm",
    # Header and footer templates are not well converted, leaving default footer so far 
    '--headerTemplate "<span></span>"',
    #'--footerTemplate "<span>Page <span class=\"pageNumber\"></span> / <span class=\"totalPages\"></span></span>"',
    paste0('"', gsub(pattern = ".md", ".html", reportPath), '"')
    )
    system(cmdLine)
  }
  
  # Use PKSim CLI to create project .pksim5
  if(saveModelFile){
    cmdLine <- paste(
    pkSimPath,
    "snap",
    # Snapshot file <modelName>.json is in working directory
    "-i", workingDirectory,
    "-o", workingDirectory,
    "-p"
    )
    system(cmdLine)

    # For next step, remove potential json from working directory
    unlink(file.path(workingDirectory, snapshotFile))
    additionalSnapshots <- ospsuite::toPathArray(additionalProjectURLs)
  for(additionalSnapshot in additionalSnapshots){
    download.file(
      # Use Github raw.githubusercontent.com to download snapshot file
      file.path(
      "https://raw.githubusercontent.com/Open-Systems-Pharmacology", 
      additionalSnapshot
      ), 
      # Keep only the last name of the path (eg <model name>_Pediatrics.json)
      destfile = file.path(workingDirectory, basename(additionalSnapshot))
      )
    warning(cmdLine)
    system(cmdLine)
    unlink(file.path(workingDirectory, basename(additionalSnapshot)))
  }
  }
  return(invisible())
}
