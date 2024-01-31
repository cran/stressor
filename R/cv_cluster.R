#' Spatial Cluster-Based Partitions for Cross-Validation
#'
#' This function creates cluster-based partitions of a sample space based on
#'   k-means clustering. Included in the function are algorithms that attempt
#'   to produce clusters of roughly equal size.
#' @param features A scaled matrix of features to be used in the clustering.
#'   Scaling usually done with \link[base]{scale} and should not include the
#'   predictor variable.
#' @param k The number of partitions for k-fold cross-validation.
#' @param k_mult k*k_mult determines the number of subgroups that will be
#'   created as part of the balancing algorithm.
#' @param ... Additional arguments passed to \link[stats]{kmeans} as needed.
#' @details More information regarding spatial cross-validation can be found in
#'  Robin Lovelace's explanation of spatial
#'  cross-validation in his
#'  \href{https://r.geocompx.org/spatial-cv.html?q=cross\%20validation#intro-cv}{textbook}.
#' @return An integer vector that is number of rows of features with indices of
#'  each group.
#' @importFrom stats kmeans
#' @examples
#'  # Creating a matrix of predictor variables
#'  x_data <- base::scale(data_gen_lm(30)[, -1])
#'  groups <- cv_cluster(x_data, 5, k_mult = 5)
#'  groups
#' @export
cv_cluster <- function(features, k, k_mult = 5, ...){

  if(k_mult %% 1 != 0 || k %% 1 != 0){
    stop("k and k_mult must be integers")
  }

  if(k_mult < 2){
    stop("k_mult must be 2 or greater (and preferably at least 5")
  }
  matrix_check(features)

  n <- nrow(features)

  # Determine what the balanced group size should be (always round up)
  ave_group <- ceiling(nrow(features) / k)

  # Article on spatial cross validation using k-means clustering:
  # - https://ieeexplore.ieee.org/stamp/stamp.jsp?arnumber=6352393&casa_token=iEKQT6SqZNoAAAAA:YpAKRz2-9gAOjkz0AX62VpXuypSK3MtOontxm0-lQJZADRhyahxlxwFt9_sdwHl-T6P-Jw1RFA&tag=1
  # Book on spatial cross validation
  # - https://geocompr.robinlovelace.net/
  kmeans_results <- kmeans(features, centers = k*k_mult, ...)

  # Extract the important stuff and eliminate kmeans vector
  features_cut <- kmeans_results$cluster
  features_sum <- kmeans_results$size
  tind <- order(features_sum, decreasing = TRUE)
  features_center <- kmeans_results$centers
  remove(kmeans_results)

  anchors <- features_center[tind[seq_len(k)], ]
  anchor_size <- features_sum[tind[seq_len(k)]]

  # stores new group memberships
  member_assign <- vector("integer", length(tind))
  member_assign[tind[seq_len(k)]] <- seq_len(k)

  for(i in tind[-seq_len(k)]){
    tdist <- apply((features_center[i, ] - t(anchors))^2, 2, sum)

    tord <- order(tdist)

    found <- FALSE
    # Place the smaller group in the closest of the largest groups that still
    # has room (below average)
    for(j in tord){
      if(anchor_size[j] + features_sum[i] <= ave_group){
        member_assign[i] <- j

        # Compute the new group size (will assign later)
        tsum <- anchor_size[j] + features_sum[i]

        # Update the centroid using a weighted average of the groups based on
        # size.
        anchors[j, ] <- (anchor_size[j]/tsum)*anchors[j, ] +
          (features_sum[i]/tsum)*features_center[i, ]

        anchor_size[j] <- tsum

        found <- TRUE
        break
      }
    }

    # If we can't find a group, just assign to the closest group and remove
    # from consideration.
    if(!found){
      member_assign[i] <- tord[1]

      tsum <- anchor_size[tord[1]] + features_sum[i]

      anchors[tord[1], ] <- (anchor_size[tord[1]]/tsum)*anchors[tord[1], ] +
        (features_sum[i]/tsum)*features_center[i, ]

      anchor_size[tord[1]] <- tsum

      anchors <- anchors[-tord[1], ]
      anchor_size <- anchor_size[-tord[1]]
    }

  }

  member_assign[features_cut]

}

# TODO: Give user option to submit a custom dist matrix



# TODO: Current Approach leaves one group out of balance, the rest above average.
# Maybe I need to not go in reverse order?


# # keeps track of group counts
# kvec <- vector("integer", k)
#
# # generates sample vector for group assignment
# ksamp <- seq_len(k)
#
# # determines how big groups "should" be
# klimit <- ceiling(sum(features_sum)/k) - median(features_sum)
#
# member_assign[features_order[ksamp]] <- ksamp
# member_center <- features_center[features_order[ksamp], ]
# kvec <- features_sum[features_order[ksamp]]
#
# full <- rep(FALSE, k)
# for(i in features_order[-seq_len(k)]){
#   tdist <- apply((features_center[i, ] - t(member_center))^2, 2, sum)
#
#   # Determine the group that has the shortest distance AND is not full.
#   tord <- order(tdist)
#   tind <- tord[!full[tord]][1]
#   member_assign[i] <- tind
#
#   # Compute a weighted average of the new centroid.
#   tnum <- kvec[tind]*member_center[tind, ] +
#     features_sum[i]*features_center[i, ]
#   tdenom <- kvec[tind] + features_sum[i]
#
#   member_center[tind, ] <- tnum / tdenom
#
#   # Update the number of observations in the group
#   kvec[tind] <- tdenom
#
#   # If we exceed the target, stop putting observations in this bin.
#   if(kvec[tind] >= klimit){
#     full[tind] <- TRUE
#   }
#
# }
#
# member_assign[features_cut]
#
#
#
