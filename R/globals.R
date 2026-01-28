


utils::globalVariables(c(
  # wv_read_txt
  "year",
  "yr",
  "mo",
  "da",
  "hr",
  "mn",
  "sc",

  # write_report_table
  "Station_Name",
  "Depl_Date", "Depl_Lat", "Depl_Lon", "Recv_Date",
  "Inst_Model", "Inst_Serial", "Inst_Depth", "Inst_Altitude",
  "Bin_Size", "First_Bin_Range", "Notes", "CMAR_ID",
  "Waves_Averaging_Interval_s",
  "Waves_Ensemble_Interval_s",
  "Waves_PingsPerEnsemble",
  "Depl_Duration",
  "Depl_Sounding",
  "Depth Sounding (m)",
  "Averaging Intervals (s)",
  "Ensemble Intervals (s)",
  "Pings per Ensemble",

  "station",
  "deployment_date",
  "deployment_sounding_m",
  "latitude",
  "longitude",
  "depl_duration",
  "retrieval_date",
  "sensor_model",
  "wave_averaging_interval_s",
  "wave_ensemble_interval_s",
  "wave_pings_per_ensemble",

  # format_report_table
  #"Record",
  "col1",

  # helpers
  "depl_date",
  "Hs",
  "H1/10",
  "H1/3",
  "Hmax",
  "Tp",
  "T1/10",
  "T1/3",
  "Tmax",
  "Dp",
  "Depth",
  "CM",
  "CD",
  "station_deployment_id",

  #  wv_plot_ts
  "variable",
  "variable_title",
  "variable_label",
  "value",
  "timestamp_utc",
 # "x",

 # wv_plot_height_rose
 "col_to_cut",
 "from_direction_degree",
 "from_direction_degree_labels",
 "ints",
 "ints_label",
 "lower",
 "n_prop",
 "significant_height_m",
 "significant_height_m_labels",
 "upper",

  # wv_test_grossrange
  "height_m",
  "period_s",
  "grossrange_flag",
  "gr_min",
  "gr_max",
  "user_min",
  "user_max",
  "height_flag",
  "period_flag",

  "wv_thresholds",

  # wv_test_rolling_sd
  "station",
  "deployment_id",
  "int_sample",
  "n_sample_effective",
  "sd_roll",
  "rolling_sd_flag",
  "rolling_sd_max",
  "n_sample",

  # adjust_variable_names
  "column_names",
  ".",

  #wv_test_rate_of_change
  "depth_diff",
  #"depth_trim_flag",

  "sensor_depth_below_surface_m",

  # wv_start_end_obs_to_trim
  "trim_obs",
  "group",

  # wv_plot_depth_flags
  #"depth_flag",

  # wv_summarise_flags
  "flag_value",
  "grossrange_flag_value",
  "n_fl",
  "n_obs",
  "qc_test",

  # wv_test_spike
  "lag_value",
  "lead_value",
  "spike_flag",
  "spike_high",
  "spike_low",
  "spike_ref",
  "spike_value",

  # wv_assign_max_flag
  "qc_col",

 # wv_compile_deployment_info
 "County",
 "Lease#",
 "Waterbody"

))
