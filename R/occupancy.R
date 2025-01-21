
#.. number of occupants -------------------------------------------------------

estimate_occupancy <- function(tfa) {
  #' @title Estimates Occupancy
  #'
  #' @description Estimates occupancy based on the given total forest area (TFA).
  #'
  #' @param tfa Total forest area.
  #'
  #' @return Estimated occupancy.
  #'
  #' @details
  #' The function calculates occupancy using a combination of base occupancy,
  #' exponential, and linear terms. The base occupancy is 1 for TFA less than 13.9,
  #' otherwise it's also 1. The exponential term is calculated based on the TFA,
  #' and the linear term is also calculated based on the TFA. The final occupancy
  #' is the sum of the base occupancy and the occupancy adjustment, which is
  #' the product of the exponential term and a constant plus the linear term.
  #'
  #' @examples
  #' estimate_occupancy(10)
  #' estimate_occupancy(20)

  # Calculate the base occupancy
  base_occupancy <- ifelse(tfa < 13.9, 1, 1)

  # Calculate the exponential term
  exponential_term <- 1 - exp(-0.000349 * (tfa - 13.9)^2)

  # Calculate the linear term
  linear_term <- 0.0013 * (tfa - 13.9)

  # Calculate the occupancy adjustment
  occupancy_adjustment <- 1.76 * exponential_term + linear_term

  # Calculate the final occupancy
  occ <- base_occupancy + occupancy_adjustment

  return(occ)
}

calculate_no_occupants <- function(idm, samp='sto',
                                   l_ehs=s_ehs_2011_ext,
                                   thrshld_ad=0.5, thrshld_ch=0.3){
  #' @title Calculates the number of occupants for a dwelling based on EUSILC assumptions.
  #'
  #' @description This function uses the European Union Statistics on Income and Living Conditions
  #' (EUSILC) assumptions to estimate the number of occupants in a dwelling
  #' 
  #' \eqn{eHS = 1 + cA (HA − 1) + cCHC}
  #'
  #' @description It considers reported occupants and a BREDEM-based estimate.
  #'
  #' @param idm Character string representing the dwelling code.
  #' @param samp Character string specifying the sample type (default "sto").
  #' @param l_ehs A data frame containing EHS data (e.g., s_ehs_2011_ext).
  #' @param thrshld_ad Numeric value representing the adult threshold (default 0.5).
  #' @param thrshld_ch Numeric value representing the child threshold (default 0.3).
  #' @return A list containing two data frames:
  #'   * `full`: Data frame with detailed occupant information.
  #'   * `simple`: Data frame with simplified occupant counts (adult, young, child, generic)

  # Data selection and filtering
  d_dwel <- l_ehs$dwelling.and.household.information
  d_comp <- l_ehs$complementary

  # Combine data frames and rename columns
  d_dwel <- d_dwel %>% dplyr::filter(V001_HousingCode==idm) %>%
    dplyr::select(V001_HousingCode, D007_OccupantsAdult, D006_OccupantsChildren)
  d_comp <- d_comp %>% dplyr::filter(V001_HousingCode==idm) %>%
    dplyr::select(V001_HousingCode, V534_TFAsurvey)

  # Define thresholds and calculate occupants
  i_thresholds <- c(thrshld_ad, thrshld_ch)

  d_occ <- plyr::join(d_dwel, d_comp, by='V001_HousingCode')
  colnames(d_occ) <- c('aacode','.occupant_adult','.occupant_child','TFA')

  # Handle missing values for adult and child occupants
  d_occ <- d_occ %>% tibble::tibble() %>%
    dplyr::mutate(occupants = rowSums(
      dplyr::across(.occupant_adult:.occupant_child), na.rm = T)) %>%
    dplyr::mutate(occupants = ifelse(occupants<=0, NA, occupants)) %>%
    dplyr::mutate(occupancy_EUSILC = 1 +
             i_thresholds[1]*(.occupant_adult - 1) +
             i_thresholds[2]*(.occupant_child),
           occupancy_BREDEM =
             ifelse(is.na(occupants),
                    round(estimate_occupancy(TFA),2), occupants)) %>%
    dplyr::mutate(.occ_adu = ifelse(is.na(.occupant_adult), 0, .occupant_adult),
           .occ_chl = ifelse(is.na(.occupant_child), 0, .occupant_child)) %>%
    dplyr::mutate(occupancy_eq = ifelse(is.na(.occupant_adult) | .occupant_adult==0,
                                 occupancy_BREDEM,occupants))

  # Subset for dwellings with missing adult and child occupants
  d_sub <- d_occ %>%
    dplyr::filter(is.na(.occupant_adult) & is.na(.occupant_child)) %>%
    dplyr::mutate(var.un = occupancy_BREDEM %% 1) %>%
    dplyr::mutate(var.up = occupancy_BREDEM - var.un) %>%
    dplyr::mutate(var.up = ifelse(var.un>0.5, var.up, var.up - 1)) %>%
    dplyr::mutate(var.un = ifelse(var.un>0.5, ceiling(var.un), ceiling(var.un)+1)) %>%
    dplyr::mutate(.occ_adu = var.up, .occ_chl = var.un) %>%
    dplyr::select(aacode, .occ_adu, .occ_chl)
  d_occ <- d_occ %>% dplyr::rows_update(d_sub, by = "aacode")

  # Update main data frame with estimated adult
  randomise_profiles(samp)
  d_occ <- d_occ %>% dplyr::rowwise() %>%
    dplyr::mutate(split = sample(seq(0,.occ_chl), 1)) %>%
    dplyr::mutate(.occupant_young = .occ_chl - split) %>%
    dplyr::mutate(.occupant_minor = split) %>%
    dplyr::select(-split,-.occupant_adult,-.occupant_child) %>%
    dplyr::rename(
      .occupant_adult = .occ_adu,
      .occupant_child = .occ_chl)

  # Rename columns and return subset
  d_ppl <- c(d_occ$.occupant_adult, d_occ$.occupant_young,
             d_occ$.occupant_minor, NA)
  d_ppl[d_ppl == 0] <- NA
  names(d_ppl) <- c('adult','young','child','generic')

  return(list(full=d_occ, simple=d_ppl))
}
