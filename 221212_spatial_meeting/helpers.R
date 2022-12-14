read_ometiff <- function(x){
    cur_files <- list.files(x, full.names = TRUE)
    cur_out <- lapply(cur_files, function(y){
        cur_image <- read.image(y)
        cur_meta <- as_list(read_xml(read.omexml(y)))
        cur_channels <- cur_meta$OME$Image$Pixels
        cur_channels <- cur_channels[names(cur_channels) != "TiffData"]
        cur_channels <- do.call("rbind.data.frame", lapply(cur_channels, attributes))
        dimnames(cur_image)$c <-  cur_channels$Name
        return(list(cur_image, cur_channels))
    })
    names(cur_out) <- list.files(x)
    
    cur_meta_x <- lapply(cur_out, "[[", 2)
    cur_meta_x <- Reduce(dplyr::intersect, cur_meta_x)

    final <- CytoImageList(lapply(cur_out, "[[", 1))
    channelData(final) <- cur_meta_x
    
    cur_mcols <- lapply(final, function(y){metadata(y)$coreMetadata})
    cur_mcols <- do.call("rbind", cur_mcols)
    
    mcols(final) <- as(cur_mcols, "DataFrame")
    
    return(final)
}

library(jsonlite)

read_omengff <- function(x, res = "0"){
    cur_files <- list.files(x, full.names = TRUE)
    cur_files <- cur_files[unlist(lapply(cur_files, file_ext)) == "zarr"]
    cur_out <- lapply(cur_files, function(y){
        cur_image <- ZarrArchive(y)
        cur_image <- dataset(x = cur_image, name = res)
        cur_image <- as(cur_image, "matrix")
        cur_image <- aperm(cur_image, perm = c(5, 4, 2, 3, 1))
        cur_meta <- fromJSON(paste0(y, "/.zattrs"))

        return(list(as(cur_image, "Image"),
                    cur_meta$omero$channels))
    })
    
    names(cur_out) <- basename(cur_files)
    
    cur_meta_x <- lapply(cur_out, "[[", 2)
    cur_meta_x <- Reduce(dplyr::intersect, cur_meta_x)
    
    final <- CytoImageList(lapply(cur_out, "[[", 1))
    channelData(final) <- cur_meta_x
    channelNames(final) <- cur_meta_x$label
    
    return(final)
}