#' @export
geom_muni <- ggregions::write_geom_region_locale(ref_data = muni_prod_geo)

#' @export
stamp_canton <- ggregions::write_stamp_region_locale(canton_geo)

#' @export
stamp_lake <- ggregions::write_stamp_region_locale(lake_geo)
