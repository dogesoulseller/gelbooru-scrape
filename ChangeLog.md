# Changelog for gelbooru-scrape
## 1.2.1.0
 - Added the `-p` option to specify a download count limit through number of pages
 - Added the `-t` option to specify max number of threads

## 1.2.0.0
 - Made it possible to download from pools

## 1.1.0.0
 - Downloading all URLs from a newline-separated text file is now possible with the `-f` option
 - Parallelism of downloading images was improved by not caring about the order they come in
 - Parallelized download link extraction
 - Made it possible to invoke the usage message through the `-h` option
